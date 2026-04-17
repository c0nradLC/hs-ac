{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Cheat
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text, pack, split, toLower)
import External
import Foreign
  ( FunPtr,
    Int32,
    Ptr,
    Storable (peek, poke),
    WordPtr (WordPtr),
    castPtrToFunPtr,
    malloc,
    nullFunPtr,
    nullPtr,
    wordPtrToPtr,
  )
import Foreign.C.Types (CBool (..), CInt (..))
import GHC.Conc.IO (threadDelay)
import GHC.Word (Word8 (..))
import Geom (deltas, getAngles, getDistance, worldToScreen)
import Global
  ( aimbotRef,
    attackFunPtrRef,
    dokillFunPtrRef,
    espRef,
    gameModeAddressRef,
    godModeRef,
    guiRef,
    infiniteammoRef,
    isVisibleFunPtrRef,
    loadedRef,
    magnetRef,
    maxPlayersAddressRef,
    noattackphysicsRef,
    originalDmgSubtractBytesRef,
    originalInfiniteAmmoBytesRef,
    originalNoattackphysicsBytesRef,
    originalSwapWindowFuncRef,
    playerAimXAddressRef,
    playerAimYAddressRef,
    playerEntityPointerRef,
    playerInCrosshairFunPtrRef,
    playerInCrosshairPtrRef,
    playerListPointerRef,
    sightKillRef,
    triggerbotRef,
  )
import Graphics.Rendering.OpenGL (BlendingFactor (OneMinusSrcAlpha, SrcAlpha), Capability (Enabled), Color (color), Color4 (Color4), ComparisonFunction (Always), HasGetter (get), MatrixMode (Projection), PrimitiveMode (LineLoop), Size (Size), Vertex (vertex), Vertex2 (Vertex2), blend, blendFunc, depthFunc, lineWidth, loadIdentity, matrixMode, ortho, preservingMatrix, renderPrimitive, viewport, ($=))
import qualified Memory as Mem
import Numeric (showHex)
import qualified Offsets
import System.Environment (lookupEnv)
import System.Posix (ProcessID, RTLDFlags (RTLD_GLOBAL, RTLD_LAZY), dlopen, dlsym)
import System.Posix.Process (getProcessID)
import Types (ACPlayer (..), ACVec (..), ImGuiRefs (_isInitialized), Player (..))
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
    -- Code cave for god-mode, this makes the player not receive damage and also deal an absurd amount of damage
    -- readIORef godModeRef >>= \active -> when active $ patchGodMode pid gameModuleBaseAddr $ gameModuleBaseAddr + Offsets.playerEntityPointer

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
      readIORef loadedRef >>= \isLoaded -> when isLoaded hack
      readIORef (_isInitialized guiRefs) >>= \isInitialized -> when isInitialized $ drawGui guiRefs
      callOriginalSwapWindow originalSwapWindow windowPtr

-- loads all the info about our local player(player1)
getLocalPlayer :: ProcessID -> IO Player
getLocalPlayer pid = do
  mPlayerEntityAddress <- readIORef playerEntityPointerRef >>= Mem.readAddress pid
  case mPlayerEntityAddress of
    Just playerEntityAddress -> do
      playerState <- fromIntegral . fromMaybe 4 <$> Mem.readInt32 pid (playerEntityAddress + Offsets.playerState)
      playerPos <- fromMaybe (0, 0, 0) <$> Mem.readVec3 pid (playerEntityAddress + Offsets.playerPos)
      playerAimX <- fromMaybe 0 <$> (readIORef playerAimXAddressRef >>= Mem.readFloat pid)
      playerAimY <- fromMaybe 0 <$> (readIORef playerAimYAddressRef >>= Mem.readFloat pid)

      -- in a DeathMatch there are still two different teams, so we need to set the player's team to be different
      -- from everyone else's so every other player gets to be an enemy
      isDeathMatch <- isDeathMatchGameMode . fromMaybe 99 <$> (readIORef gameModeAddressRef >>= Mem.readInt32 pid)
      playerTeam <-
        if isDeathMatch
          then return 4
          else fromIntegral . fromMaybe 4 <$> Mem.readInt32 pid (playerEntityAddress + Offsets.playerTeam)

      return
        ( Player
            { _pos = playerPos,
              _state = playerState,
              _team = playerTeam,
              _distance = Nothing,
              _visible = False,
              _aimX = playerAimX,
              _aimY = playerAimY,
              _baseAddr = playerEntityAddress
            }
        )
    Nothing -> error "Error trying to read the Player's entity address."

{- maps through the player list and loads all the players info into a list
  only obtains alive players (state == 0)
  also calculates the distance of each player/bot from our localPlayer
  and also calls isvisible to set _visible on each player/bot
-}
getPlayersList :: ProcessID -> (Float, Float, Float) -> IO [Player]
getPlayersList pid playerPos@(playerX, playerY, playerZ) = do
  mMaxPlayers <- readIORef maxPlayersAddressRef >>= Mem.readInt32 pid
  mPlayersListAddress <- readIORef playerListPointerRef >>= Mem.readAddress pid

  case (mMaxPlayers, mPlayersListAddress) of
    (Just maxPlayers, Just playersList) -> do
      catMaybes
        <$> mapM
          ( \botPointer -> do
              mBotAddress <- Mem.readAddress pid botPointer
              case mBotAddress of
                Just botAddress -> do
                  botState <-
                    fromMaybe 4 <$> Mem.readInt32 pid (botAddress + Offsets.playerState)
                  botPos@(botX, botY, botZ) <-
                    fromMaybe (0, 0, 0) <$> Mem.readVec3 pid (botAddress + Offsets.playerPos)
                  botTeam <-
                    fromMaybe 4 <$> Mem.readInt32 pid (botAddress + Offsets.playerTeam)

                  if botState == 0 -- when bot is alive
                    then do
                      -- allocate a ptr in memory and put the player's pos vec in it
                      playerVecPtr <-
                        malloc
                          >>= \ptr -> ptr <$ (poke ptr $ ACVec {_x = realToFrac playerX, _y = realToFrac playerY, _z = realToFrac playerZ})

                      -- allocate a ptr in memory and put the bot's pos vec in it
                      botVecPtr <-
                        malloc
                          >>= \ptr -> ptr <$ (poke ptr $ ACVec {_x = realToFrac botX, _y = realToFrac botY, _z = realToFrac botZ})

                      -- call our isvisible C bridge
                      botIsVisible <-
                        readIORef isVisibleFunPtrRef
                          >>= \funPtr -> isvisible funPtr playerVecPtr botVecPtr nullPtr 0

                      let (deltaX, deltaY, _) = deltas playerPos botPos
                      return $
                        Just
                          Player
                            { _pos = botPos,
                              _state = 0,
                              _team = fromIntegral botTeam,
                              _distance = Just $ getDistance (deltaX, deltaY),
                              _visible = botIsVisible == 1, -- CBool is just an int
                              _aimX = 0, -- We won't use a bot's aim coords for anything
                              _aimY = 0,
                              _baseAddr = botAddress
                            }
                    else return Nothing
                Nothing -> return Nothing
          )
          (botsPointers (playersList + 0x8) (playersList + 0x10) maxPlayers)
    (_, _) -> error "Error trying to read max players and players list addresses."

-- dominates the game each frame
hack :: IO ()
hack = do
  pid <- getProcessID

  localPlayer <- getLocalPlayer pid
  playersList <- getPlayersList pid (_pos localPlayer)

  -- Magnet
  playersList <- readIORef magnetRef >>= \active -> if active then magnet pid localPlayer playersList else return playersList

  -- call playerincrosshair and store the player ptr in IORef
  readIORef playerInCrosshairFunPtrRef >>= playerincrosshair >>= writeIORef playerInCrosshairPtrRef

  -- Sight-kill - Never heard of this before so this is the name I came up with
  --  instantly kills whatever enemy crosses my crosshair
  readIORef sightKillRef >>= \active -> when active $ sightKill localPlayer

  -- ESP
  readIORef espRef >>= \active -> when active $ drawESP localPlayer (_aimX localPlayer) (_aimY localPlayer) playersList

  -- Triggerbot
  readIORef triggerbotRef >>= \active -> when active $ triggerBot localPlayer

  -- Aimbot
  readIORef aimbotRef >>= \active -> when active $ aimbot pid localPlayer playersList
