module Main where

import Data.List (find, minimumBy)
import qualified Memory as Mem
import Numeric (readHex, showHex)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B8
import Data.Word
import GHC.Int (Int32)
import Control.Monad (forever)
import Data.Ord (comparing)

main :: IO ()
main = do
    mbPid <- Mem.findProcessId "linux_64_client"
    case mbPid of
        Just pid -> do
            let procPath = "/proc/" ++ show pid
            modules <- Mem.getProcessModules $ procPath ++ "/maps"
            gameModuleBaseAddr <- case find (\mod -> Mem._name mod == "linux_64_client") modules of
                Just gameModule -> do
                    return $ Mem._baseAddr gameModule
                Nothing -> do
                    error "Game/Binary module not found."
            let playerEntityPointer = gameModuleBaseAddr + (fst . head $ readHex "19d518")
            let playerListPointer = gameModuleBaseAddr + (fst . head $ readHex "19d520")
            let maxPlayersAddress = gameModuleBaseAddr + (fst . head $ readHex "19d52C")

            mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointer

            case mPlayerEntityAddress of
                Just playerEntityAddr -> do
                    let healthAddr = playerEntityAddr + (fst . head $ readHex "100")
                    let ammoAddr = playerEntityAddr + (fst . head $ readHex "154")
                    let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
                    let playerAimXAddr = playerEntityAddr + (fst . head $ readHex "38")
                    let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
                    let recoilInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "77a9c")
                    let spreadInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fafc9")
                    let attackPhysicsAddr = gameModuleBaseAddr + (fst . head $ readHex "faf20")

                    --Mem.writeMemoryBytes pid recoilInstrAddr (replicate 6 0x90)
                    --Mem.writeMemoryBytes pid spreadInstrAddr (replicate 6 0x90)

                    Mem.writeMemoryBytes pid attackPhysicsAddr (replicate 1 0xc3)

                    forever $ do
                        print $ show pid
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
                                                    if _team bot /= _team localPlayer && _state bot == 0 then do
                                                        print $ "Bot Address: " ++ showHex botAddress ""
                                                        return $ Just bot
                                                    else return Nothing
                                                Nothing -> return Nothing
                                            ) botPointers

                                        let mTarget = getClosestBot bots
                                        case mTarget of
                                            Just target -> do
                                                let (aimX, aimY) = getAngles (_pos localPlayer) (_pos target)
                                                Mem.writeFloat pid playerAimYAddr aimY
                                                Mem.writeFloat pid playerAimXAddr aimX
                                                return ()
                                            Nothing -> return ()
                                        return ()
                                    Nothing -> return ()
                            Nothing -> return ()
                Nothing -> return ()
        Nothing -> do
            print "pid not found."

getBotsPointers :: Word64 -> Word64 -> Int32 -> [Word64]
getBotsPointers fstAddr sndAddr maxPlayers =
    [fstAddr + (fromIntegral i * nextBotOffset) | i <- [0 .. ((maxPlayers `div` 2) - 1)]] ++
    [sndAddr + (fromIntegral i * nextBotOffset) | i <- [0 .. ((maxPlayers `div` 2) - 2)]]

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