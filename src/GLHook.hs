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
import Data.List (isInfixOf, find, minimumBy)
import Control.Concurrent
import System.Posix.DynamicLinker
import qualified Memory as Mem
import qualified Data.ByteString as BS
import Data.Word
import GHC.Int (Int32)
import Numeric (readHex, showHex)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (get)

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

    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity

    test <- get GL.viewport
    print $ show test

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
            let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
            let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
            let playerAimXAddr = playerEntityAddr + (fst . head $ readHex "38")
            
            mPlayerPos <- Mem.readVec3 pid playerPosAddr
            
            -- Set primary ammo
            Mem.writeMem memPath ammoAddr 1337
            --test1 <- Mem.readInt32 pid ammoAddr
            --print $ "AmmoAddr val: " ++ show test1

            -- Set health
            Mem.writeMem memPath healthAddr 1337
            --test2 <- Mem.readInt32 pid healthAddr
            --print $ "HealthAddr val: " ++ show test2

            mMaxPlayers <- Mem.readInt32 pid maxPlayersAddress
            mPlayerState <- Mem.readInt32 pid (playerEntityAddr + playerStateOffset)
            mPlayerPos <- Mem.readVec3 pid (playerEntityAddr + playerPosOffset)
            mPlayerTeam <- Mem.readInt32 pid (playerEntityAddr + playerTeamOffset)

            let playerState = fromMaybe 3 mPlayerState
                playerPos = fromMaybe (0,0,0) mPlayerPos
                playerTeam = fromMaybe 3 mPlayerTeam

            let localPlayer = Player {_pos = playerPos, _state = playerState, _team = playerTeam, _distance = Nothing}
            case mMaxPlayers of
                Just maxPlayers -> do
                    mPlayersListAddress <- Mem.readAddress pid playerListPointer
                    case mPlayersListAddress of
                        Just playersListAddr -> do
                            let firstBotAddress = playersListAddr + (fst . head $ readHex "8")
                            let sndBotAddress = playersListAddr + (fst . head $ readHex "10")
                            let botPointers = getBotsPointers firstBotAddress sndBotAddress maxPlayers

                            bots <- mapM (\botPointer -> do
                                mBotAddress <- Mem.readAddress pid botPointer
                                case mBotAddress of
                                    Just botAddress -> do
                                        mBotState <- Mem.readInt32 pid (botAddress + playerStateOffset)
                                        mBotPos <- Mem.readVec3 pid (botAddress + playerPosOffset)
                                        mBotTeam <- Mem.readInt32 pid (botAddress + playerTeamOffset)
                                        let botState = fromMaybe 4 mBotState
                                            botPos = fromMaybe (0,0,0) mBotPos
                                            botTeam = fromMaybe 4 mBotTeam
                                            (deltaX, deltaY, _) = getDeltas (_pos localPlayer) (_pos bot)
                                            bot = Player
                                                { _pos = botPos
                                                , _state = botState
                                                , _team = botTeam
                                                , _distance = Just $ getDistance (deltaX, deltaY)
                                                }
                                        if _team bot /= _team localPlayer && _state bot == 0 then
                                            return $ Just bot
                                        else return Nothing
                                    Nothing -> return Nothing
                                ) botPointers
                            let mTarget = getClosestBot bots
                            case mTarget of
                                Just target -> do
                                    let (aimX, aimY) = getAngles (_pos localPlayer) (_pos target)
                                    Mem.writeFloat memPath playerAimYAddr aimY
                                    Mem.writeFloat memPath playerAimXAddr aimX
                                    return ()
                                Nothing -> return ()
                            return ()
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

getAngles :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float)
getAngles playerPos botPos = do
    let (deltaX, deltaY, deltaZ) = getDeltas playerPos botPos

        yawRad = atan2 deltaX (-deltaY)
        yawDeg = yawRad * 180.0 / pi
        yawDeg' = if yawDeg < 0 then yawDeg + 360.0 else yawDeg

        horizontalDistance = getDistance (deltaX, deltaY)

        pitchRad = atan2 deltaZ horizontalDistance
        pitchDeg = pitchRad * 180.0 / pi
    (yawDeg', pitchDeg)

getDistance :: (Float, Float) -> Float
getDistance (deltaX, deltaY) = sqrt ((deltaX * deltaX) + (deltaY * deltaY))

getDeltas :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
getDeltas (playerX, playerY, playerZ) (botX, botY, botZ) = (botX - playerX, botY - playerY, botZ - playerZ)

getClosestBot :: [Maybe Player] -> Maybe Player
getClosestBot ms =
  let candidates = [(p, d) | Just p <- ms, Just d <- [_distance p]]
  in if null candidates then Nothing else Just (fst (minimumBy (comparing snd) candidates))

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
    { _pos      :: (Float, Float, Float)
    , _team     :: Int32
    , _state    :: Int32
    , _distance :: Maybe Float
    }
    deriving Show