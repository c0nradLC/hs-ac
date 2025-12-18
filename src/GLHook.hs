{-# LANGUAGE ForeignFunctionInterface #-}

module GLHook where

import Foreign.C
import Foreign.Ptr
import Foreign.C.String
import System.Posix.Process
import System.Posix.Files
import System.Directory
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Unsafe
import Data.List (isInfixOf, find)
import Control.Concurrent
import System.Posix.DynamicLinker
import qualified Memory as Mem
import qualified Data.ByteString as BS
import Data.Word
import GHC.Int (Int32)
import Numeric (readHex, showHex)

-- SDL2 Window type (opaque pointer)
type SDL_Window = Ptr ()

-- Our hook for SDL_GL_SwapWindow
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: SDL_Window -> IO ()

sdlGLSwapWindowHook :: SDL_Window -> IO ()
sdlGLSwapWindowHook _ = do

    processId <- getProcessID
    let pid = fromIntegral processId
        procPath = "/proc/" ++ show pid
        memPath = procPath ++ "/mem"
    modules <- Mem.getProcessModules $ procPath ++ "/maps"
    gameModuleBaseAddr <- case find (\mod -> Mem._name mod == "linux_64_client") modules of
        Just gameModule -> do
            return $ Mem._baseAddr gameModule
        Nothing -> do
            error "Game/Binary module not found."

    let playerEntityPointer = gameModuleBaseAddr + (fst . head $ readHex "19d518")
    let playerListPointer = gameModuleBaseAddr + (fst . head $ readHex "19d520")
    let maxPlayersAddress = gameModuleBaseAddr + (fst . head $ readHex "19d52C")
    let ammoInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fd06e")
    let recoilInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "77a9c")
    -- Need to revisit how the knockback works, it might not be this, maybe instead of NOP we need to patch it differently
    -- let knockbackInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fcf6d")

    -- Patch Infinite ammo
    --ammoInstrVal <- Mem.readInstruction pid ammoInstrAddr 3
    Mem.writeInstruction (fromIntegral pid) ammoInstrAddr (BS.pack $ replicate 3 0x90)
    -- Rollback patch
    --Mem.writeInstruction pid ammoInstrAddr $ fromMaybe (B8.pack "") ammoInstrVal

    -- No recoil
    --noRecoilInstrVal <- Mem.readInstruction pid recoilInstrAddr 6
    Mem.writeInstruction pid recoilInstrAddr (BS.pack $ replicate 6 0x90)
    -- Rollback patch
    --Mem.writeInstruction pid recoilInstrAddr $ fromMaybe (B8.pack "") noRecoilInstrVal

    -- No knockback
    --Mem.writeInstruction pid knockbackInstrAddr (BS.pack $ replicate 7 090)
    --test5 <- Mem.readInstruction pid knockbackInstrAddr 8
    --print $ "knockback val: " ++ show test5

    mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointer
    case mPlayerEntityAddress of
        Just playerEntityAddr -> do 
            let healthAddr = playerEntityAddr + (fst . head $ readHex "100")
            let ammoAddr = playerEntityAddr + (fst . head $ readHex "154")
            --let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
            let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
            
            mPlayerPos <- Mem.readVec3 pid playerPosAddr
            print $ "Player position: " ++ show mPlayerPos
            
            -- Set primary ammo
            Mem.writeMem memPath ammoAddr 1337
            --test1 <- Mem.readInt32 pid ammoAddr
            --print $ "AmmoAddr val: " ++ show test1

            -- Set health
            Mem.writeMem memPath healthAddr 1337
            --test2 <- Mem.readInt32 pid healthAddr
            --print $ "HealthAddr val: " ++ show test2

            mMaxPlayers <- Mem.readInt32 pid maxPlayersAddress

            playerState <- Mem.readInt32 pid (playerEntityAddr + playerStateOffset)
            playerPos <- Mem.readVec3 pid (playerEntityAddr + playerPosOffset)
            playerTeam <- Mem.readInt32 pid (playerEntityAddr + playerTeamOffset)

            let localPlayer = Player {_pos = playerPos, _state = playerState, _team = playerTeam}
            case mMaxPlayers of
                Just maxPlayers -> do
                    mPlayersListAddress <- Mem.readAddress pid playerListPointer
                    case mPlayersListAddress of
                        Just playersListAddr -> do
                            let firstBotAddress = playersListAddr + (fst . head $ readHex "8")
                            let sndBotAddress = playersListAddr + (fst . head $ readHex "10")
                            let botPointersTest = getBotsPointers firstBotAddress sndBotAddress maxPlayers

                            mapM_ (\botPointer -> do
                                    mBotAddress <- Mem.readAddress pid botPointer
                                    case mBotAddress of 
                                        Just botAddress -> do
                                            botState <- Mem.readInt32 pid (botAddress + playerStateOffset)
                                            botPos <- Mem.readVec3 pid (botAddress + playerPosOffset)
                                            botTeam <- Mem.readInt32 pid (botAddress + playerTeamOffset)

                                            let bot = Player {_pos = botPos, _state = botState, _team = botTeam}
                                            print $ show bot
                                            return ()
                                        Nothing -> return ()
                                    ) botPointersTest
                        Nothing -> return ()
                Nothing -> return ()
        Nothing -> return()
    
    return ()

-- Function pointer callers
foreign import ccall "dynamic" 
    callSwapWindow :: FunPtr (SDL_Window -> IO ()) -> SDL_Window -> IO ()

getBotsPointers :: Word64 -> Word64 -> Int32 -> [Word64]
getBotsPointers fstAddr sndAddr maxPlayers =
    [fstAddr + fromIntegral i * nextBotOffset | i <- [0 .. maxPlayers-3]] ++
    [sndAddr + fromIntegral i * nextBotOffset | i <- [0 .. maxPlayers-4]]

nextBotOffset :: Word64
nextBotOffset = fst . head $ readHex "10"

playerTeamOffset :: Word64
playerTeamOffset =  fst . head $ readHex "320"

playerStateOffset :: Word64
playerStateOffset = fst . head $ readHex "32C"

playerPosOffset :: Word64
playerPosOffset = fst . head $ readHex "8"

playerNameOffset :: Word64
playerNameOffset = fst . head $ readHex "219"

data Player
    = Player
    { _pos   :: Maybe (Float, Float, Float)
    , _team  :: Maybe Int32
    , _state :: Maybe Int32
    }
    deriving Show