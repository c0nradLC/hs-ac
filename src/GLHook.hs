{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Foreign.Ptr
import System.Posix.Process
import Data.List (find, minimumBy)
import qualified Memory as Mem
import qualified Data.ByteString as BS
import Data.Word
import GHC.Int (Int32)
import Numeric (readHex)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, catMaybes)
import Graphics.Rendering.OpenGL (Size(..), matrixMode, loadIdentity, HasSetter (($=)), MatrixMode (Projection), ortho, HasGetter (get), viewport, lineWidth, ComparisonFunction (Always), Capability (Enabled), depthFunc, Color (color), Color4 (Color4), renderPrimitive, PrimitiveMode (LineLoop), Vertex2 (Vertex2), Vertex (vertex), preservingMatrix, BlendingFactor (SrcAlpha, OneMinusSrcAlpha), blendFunc, blend)
import Foreign.Storable (Storable(poke))
import Data.List.NonEmpty (fromList)
import Control.Concurrent (forkIO)

-- Our hook for SDL_SwapWindow
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: Ptr () -> IO ()

sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook _ = do

    processId <- getProcessID
    let pid = fromIntegral processId
        procPath = "/proc/" ++ show pid
    modules <- Mem.getProcessModules $ procPath ++ "/maps"
    gameModuleBaseAddr <- case find (\modl -> Mem._name modl == "linux_64_client") modules of
        Just gameModule -> do
            return $ Mem._baseAddr gameModule
        Nothing -> do
            error "Game/Binary module not found."

    let playerEntityPointer = gameModuleBaseAddr + (fst . head $ readHex "19d518")
    let playerListPointer = gameModuleBaseAddr + (fst . head $ readHex "19d520")
    let maxPlayersAddress = gameModuleBaseAddr + (fst . head $ readHex "19d52C")
    let ammoInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fd06e")
    let recoilInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "77a9c")
    let spreadInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fafc9")

    -- Patch Infinite ammo
    --ammoInstrVal <- Mem.readInstruction pid ammoInstrAddr 3
    Mem.writeMemoryBytes (fromIntegral pid) ammoInstrAddr (replicate 3 0x90)
    -- Rollback patch
    --Mem.writeInstruction pid ammoInstrAddr $ fromMaybe (B8.pack "") ammoInstrVal

    -- No recoil
    --noRecoilInstrVal <- Mem.readInstruction pid recoilInstrAddr 6
    Mem.writeMemoryBytes pid recoilInstrAddr (replicate 6 0x90)
    -- Rollback patch
    --Mem.writeInstruction pid recoilInstrAddr $ fromMaybe (B8.pack "") noRecoilInstrVal

    -- No spread and no kickback
    Mem.writeMemoryBytes pid spreadInstrAddr (replicate 6 0x90)
    --test5 <- Mem.readInstruction pid knockbackInstrAddr 8
    --print $ "knockback val: " ++ show test5

    mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointer
    case mPlayerEntityAddress of
        Just playerEntityAddr -> do
            let healthAddr = playerEntityAddr + (fst . head $ readHex "100")
            let assaultAmmoAddr = playerEntityAddr + (fst . head $ readHex "154")
            let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
            let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
            let playerAimXAddr = playerEntityAddr + (fst . head $ readHex "38")

            -- Set primary ammo
            Mem.writeInt pid assaultAmmoAddr 1337
            --test1 <- Mem.readInt32 pid ammoAddr
            --print $ "AmmoAddr val: " ++ show test1

            -- Set health
            Mem.writeInt pid healthAddr 1337
            --test2 <- Mem.readInt32 pid healthAddr
            --print $ "HealthAddr val: " ++ show test2

            mMaxPlayers <- Mem.readInt32 pid maxPlayersAddress
            mPlayerState <- Mem.readInt32 pid (playerEntityAddr + playerStateOffset)
            mPlayerPos <- Mem.readVec3 pid playerPosAddr
            mPlayerTeam <- Mem.readInt32 pid (playerEntityAddr + playerTeamOffset)

            let playerState = fromMaybe 4 mPlayerState
                playerPos = fromMaybe (0,0,0) mPlayerPos
                playerTeam = fromMaybe 4 mPlayerTeam

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
                                    Mem.writeFloat pid playerAimYAddr aimY
                                    Mem.writeFloat pid playerAimXAddr aimX
                                    return ()
                                Nothing -> return ()
                            aimX <- Mem.readFloat pid playerAimXAddr
                            aimY <- Mem.readFloat pid playerAimYAddr
                            drawESP
                                (_pos localPlayer)
                                (fromMaybe 0.0 aimX)
                                (fromMaybe 0.0 aimY)
                                (catMaybes bots)
                            return ()
                        Nothing -> return ()
                Nothing -> return ()
        Nothing -> return ()

    return ()

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

degToRad :: Float -> Float
degToRad deg = deg * pi / 180.0

normalizeDelta :: Float -> Float
normalizeDelta ang
  | ang <= -pi = normalizeDelta (ang + 2 * pi)
  | ang > pi   = normalizeDelta (ang - 2 * pi)
  | otherwise  = ang

worldToScreen :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Maybe (Float, Float, Float)
worldToScreen playerPos aimXDeg aimYDeg screenW screenH enemyPos =
    let (deltaX, deltaY, deltaZ) = getDeltas playerPos enemyPos
        horizDist = getDistance (deltaX, deltaY)
        yawToRad = atan2 deltaX (-deltaY)
        pitchToRad = atan2 deltaZ horizDist
        footPitchToRad = atan2 (deltaZ - 4.5) horizDist -- We know that the aimY normally is 4.5 by looking at its value through CE
        camYawRad = degToRad aimXDeg
        camPitchRad = degToRad aimYDeg
        deltaYaw = normalizeDelta (yawToRad - camYawRad)
        deltaPitch = normalizeDelta (pitchToRad - camPitchRad)
        footDeltaPitch = normalizeDelta (footPitchToRad - camPitchRad)
        dotProd = cos deltaYaw * cos deltaPitch
        halfHfovRad = degToRad (90 / 2)
        aspectRatio = screenH / screenW
        halfVfovRad = atan (tan halfHfovRad * aspectRatio)
        screenX = screenW / 2 + (screenW / 2) * (tan deltaYaw / tan halfHfovRad)
        screenY = screenH / 2 - (screenH / 2) * (tan deltaPitch / tan halfVfovRad)
        footScreenY = screenH / 2 - (screenH / 2) * (tan footDeltaPitch / tan halfVfovRad)
    in
    if notOnScreen horizDist dotProd deltaYaw deltaPitch screenX screenW screenY screenH then Nothing
    else Just (screenX, screenY, footScreenY)
    where notOnScreen horizDist dotProd deltaYaw deltaPitch screenX sw screenY sh =
            horizDist < 0.01 || dotProd < 0 || abs deltaYaw > pi / 1.8 || abs deltaPitch > pi / 1.8 || screenX < -100 || screenX > sw + 100 || screenY < -100 || screenY > sh + 100

-- Draw box on bot
drawBox :: (Float, Float, Float) -> IO ()
drawBox (posX, posY, footPosY) = do
  let boxHeight = posY - footPosY
      boxWidth = boxHeight * 0.45
      left = (posX + posX) / 2 - boxWidth / 2
      right = left + boxWidth
  color $ Color4 0 0 1 (0.8 :: Float)
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 left footPosY
    vertex $ Vertex2 right footPosY
    vertex $ Vertex2 right posY
    vertex $ Vertex2 left posY

drawESP :: (Float, Float, Float) -> Float -> Float -> [Player] -> IO ()
drawESP playerPos aimX aimY enemies =
    preservingMatrix $ do

    -- Get current viewport (Position/Size)
    vp <- get viewport
    let (_, Size vw vh) = vp  -- Ignore Position x/y (0,0)

    -- 2D overlay setup
    matrixMode $= Projection
    loadIdentity
    ortho 0 (realToFrac vw) (realToFrac vh) 0 (-1) 1

    -- Overlay states
    depthFunc $= Just Always
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 2.5

    -- Draw boxes
    mapM_ (\bot -> do
            let (botX, botY, botFootY) = fromMaybe (0, 0, 0) (worldToScreen playerPos aimX aimY (realToFrac vw) (realToFrac vh) (_pos bot))
            drawBox (botX, botY, botFootY))
        enemies

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