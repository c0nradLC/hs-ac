{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Cheat
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import External
import Foreign
  ( Ptr,
    WordPtr (WordPtr),
    castPtrToFunPtr,
    nullFunPtr,
    wordPtrToPtr,
  )
import GHC.Conc.IO (threadDelay)
import Global
  ( attackFunPtrRef,
    dokillFunPtrRef,
    gameModeAddressRef,
    gameModuleBaseAddrRef,
    guiRef,
    isVisibleFunPtrRef,
    loadedRef,
    maxPlayersAddressRef,
    originalDmgSubtractBytesRef,
    originalInfiniteAmmoBytesRef,
    originalNoattackphysicsBytesRef,
    originalSwapWindowFuncRef,
    pidRef,
    playerAimXAddressRef,
    playerAimYAddressRef,
    playerEntityPointerRef,
    playerInCrosshairFunPtrRef,
    playerListPointerRef,
  )
import qualified Memory as Mem
import qualified Offsets
import System.Posix (RTLDFlags (RTLD_GLOBAL, RTLD_LAZY), dlopen, dlsym)
import System.Posix.Process (getProcessID)
import Types (ImGuiRefs (_isInitialized))
import Window (drawGui, initDearImGuiWindow)

-- patchClient
foreign export ccall "patchClient" patchClient :: IO ()

-- loads all the refs (static addresses) and patches the binary, what's in here gets called only once on startup after 1 second
patchClient :: IO ()
patchClient = do
  _ <- forkIO $ do
    threadDelay 1000000 -- 1 second
    pid <- getProcessID
    gameModuleBaseAddr <- Mem.getGameModuleBaseAddr $ "/proc/" ++ show pid ++ "/maps"

    writeIORef pidRef pid
    writeIORef gameModuleBaseAddrRef gameModuleBaseAddr
    writeIORef playerEntityPointerRef $ gameModuleBaseAddr + Offsets.playerEntityPointer

    mPlayerEntityAddress <- Mem.readAddress pid (gameModuleBaseAddr + Offsets.playerEntityPointer)
    case mPlayerEntityAddress of
      Just playerEntityAddress -> do
        writeIORef playerAimYAddressRef $ playerEntityAddress + Offsets.playerAimY
        writeIORef playerAimXAddressRef $ playerEntityAddress + Offsets.playerAimX
      Nothing -> return ()

    writeIORef playerListPointerRef $ gameModuleBaseAddr + Offsets.playerListPointer
    writeIORef maxPlayersAddressRef $ gameModuleBaseAddr + Offsets.maxPlayers
    writeIORef gameModeAddressRef $ gameModuleBaseAddr + Offsets.gameMode

    -- function ptrs
    writeIORef isVisibleFunPtrRef $ castPtrToFunPtr $ wordPtrToPtr $ WordPtr $ gameModuleBaseAddr + Offsets.isVisibleFunction
    writeIORef attackFunPtrRef $ castPtrToFunPtr $ wordPtrToPtr $ WordPtr $ gameModuleBaseAddr + Offsets.attackFunction
    writeIORef playerInCrosshairFunPtrRef $ castPtrToFunPtr $ wordPtrToPtr $ WordPtr $ gameModuleBaseAddr + Offsets.playerInCrosshairFunction
    writeIORef dokillFunPtrRef $ castPtrToFunPtr $ wordPtrToPtr $ WordPtr $ gameModuleBaseAddr + Offsets.doKillFunction

    -- original bytes for the instructions that consumes/deducts our ammo when we shoot
    Mem.readBytes pid (gameModuleBaseAddr + Offsets.consumeAmmoInstr) 3 >>= writeIORef originalInfiniteAmmoBytesRef

    -- original bytes for the NoAttackPhysics function call instruction when shooting
    Mem.readBytes pid (gameModuleBaseAddr + Offsets.attackPhysicsFunction) 1 >>= writeIORef originalNoattackphysicsBytesRef

    -- original bytes for the dmg subtraction instruction when shooting, where we patch with the jump to our code cave
    Mem.readBytes pid (gameModuleBaseAddr + Offsets.dmgSubtract) 6 >>= writeIORef originalDmgSubtractBytesRef

    writeIORef loadedRef True
  return ()

-- Our SwapWindow hook
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: Ptr () -> IO ()

{- our actual hook for SDL_GL_SwapWindow implementatino that gets called by our C wrapper
  on startup, it obtains the address for the SDL_GL_SwapWindow symbol from the loaded libraries
  (libSDL2-2.0.so in this case) and stores it in IORef.
  after it is loaded into its IORef, checks if patchClient has already been called (by checking isLoaded)
  if it has already been patched/loaded then calls our hack funtions and then calls the original
  SDL_GL_SwapWindow that we stored in IORef.
-}
sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook windowPtr = do
  guiRefs <- readIORef guiRef
  isGuiInitialized <- readIORef $ _isInitialized guiRefs
  unless isGuiInitialized $ initDearImGuiWindow guiRefs windowPtr
  swapWindowR <- readIORef originalSwapWindowFuncRef
  pid <- readIORef pidRef
  gameModuleBaseAddr <- readIORef gameModuleBaseAddrRef
  let originalSwapWindow = fromMaybe nullFunPtr swapWindowR
  if originalSwapWindow == nullFunPtr
    then do
      {- TODO: Fix this, should use fallbacks for lib name,
      same lib in Arch didn't have the trailing ".0" -}
      dl <- dlopen "libSDL2-2.0.so.0" [RTLD_LAZY, RTLD_GLOBAL]
      original_SwapWindow <- dlsym dl "SDL_GL_SwapWindow"
      unless (original_SwapWindow == nullFunPtr) $ do
        writeIORef originalSwapWindowFuncRef $ Just original_SwapWindow
    else do
      readIORef loadedRef >>= \isLoaded -> when isLoaded $ hack pid
      readIORef (_isInitialized guiRefs) >>= \isInitialized ->
        when isInitialized $ drawGui guiRefs pid gameModuleBaseAddr
      callOriginalSwapWindow originalSwapWindow windowPtr
