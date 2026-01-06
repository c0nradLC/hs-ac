{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Foreign.Ptr ( Ptr )
import System.Posix.Process ( getProcessID )
import Data.List (minimumBy)
import qualified Memory as Mem
import Data.Word ( Word64 )
import GHC.Int (Int32)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, catMaybes)
import Graphics.Rendering.OpenGL (Size(..), matrixMode, loadIdentity, HasSetter (($=)), MatrixMode (Projection), ortho, HasGetter (get), viewport, lineWidth, ComparisonFunction (Always), Capability (Enabled), depthFunc, Color (color), Color4 (Color4), renderPrimitive, PrimitiveMode (LineLoop), Vertex2 (Vertex2), Vertex (vertex), preservingMatrix, BlendingFactor (SrcAlpha, OneMinusSrcAlpha), blendFunc, blend)
import Foreign.C.Types (CFloat(..), CBool(..), CUInt(..))
import Control.Monad (when)
import Memory (getGameModuleBaseAddr)
import qualified Offsets

-- Our hooks
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: Ptr () -> IO ()

foreign import ccall unsafe "IsVisible" isVisible :: CUInt -> CFloat -> CFloat -> CFloat
                                                -> CFloat -> CFloat -> CFloat -> IO CBool

sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook _ = do
    pid <- getProcessID
    gameModuleBase <- getGameModuleBaseAddr $ "/proc/" ++ show pid ++ "/maps"

    let playerEntityPointer = gameModuleBase + Offsets.playerEntityPointer
    let playerListPointer = gameModuleBase + Offsets.playerListPointer
    let maxPlayersAddress = gameModuleBase + Offsets.maxPlayers
    let ammoInstrAddress = gameModuleBase + Offsets.ammoInstr
    let attackPhysicsFunctionAddress = gameModuleBase + Offsets.attackPhysicsFunction
    let isVisibleFunctionAddress = gameModuleBase + Offsets.isVisibleFunction

    -- Infinite ammo
    Mem.writeMemoryBytes pid ammoInstrAddress (replicate 3 0x90)

    -- NoSpread, NoRecoil and NoKickback
    Mem.writeMemoryBytes pid attackPhysicsFunctionAddress (replicate 1 0xc3)

    mPlayerEntityess <- Mem.readAddress pid playerEntityPointer
    case mPlayerEntityess of
        Just playerEntity -> do
            let assaultAmmoAddress = playerEntity + Offsets.primaryWeaponAmmo
            let playerHealthAddress = playerEntity + Offsets.playerHealth
            let playerPosAddress = playerEntity + Offsets.playerPos
            let playerAimYAddress = playerEntity + Offsets.playerAimY
            let playerAimXAddress = playerEntity + Offsets.playerAimX

            -- Set primary ammo
            Mem.writeInt pid assaultAmmoAddress 9999

            -- Set health
            Mem.writeInt pid playerHealthAddress 9999

            mMaxPlayers <- Mem.readInt32 pid maxPlayersAddress
            mPlayerState <- Mem.readInt32 pid (playerEntity + Offsets.playerState)
            mPlayerPos <- Mem.readVec3 pid playerPosAddress
            mPlayerTeam <- Mem.readInt32 pid (playerEntity + Offsets.playerTeam)

            let playerPos@(playerX, playerY, playerZ) = fromMaybe (0,0,0) mPlayerPos
            let localPlayer = Player {_pos = playerPos, _state = fromMaybe 4 mPlayerState, _team = fromMaybe 4 mPlayerTeam, _distance = Nothing, _visible = False}

            case mMaxPlayers of
                Just maxPlayers -> do
                    mPlayersListess <- Mem.readAddress pid playerListPointer
                    case mPlayersListess of
                        Just playersList -> do
                            mBots <- mapM (\botPointer -> do
                                mBotess <- Mem.readAddress pid botPointer
                                case mBotess of
                                    Just botess -> do
                                        mBotState <- Mem.readInt32 pid (botess + Offsets.playerState)
                                        mBotPos <- Mem.readVec3 pid (botess + Offsets.playerPos)
                                        mBotTeam <- Mem.readInt32 pid (botess + Offsets.playerTeam)
                                        let botState = fromMaybe 4 mBotState
                                            botPos@(botX, botY, botZ) = fromMaybe (0,0,0) mBotPos
                                            botTeam = fromMaybe 4 mBotTeam
                                            (deltaX, deltaY, _) = deltas (_pos localPlayer) botPos
                                        botIsVisible <- isVisible (fromIntegral isVisibleFunctionAddress) (realToFrac playerX) (realToFrac playerY) (realToFrac playerZ) (realToFrac botX) (realToFrac botY) (realToFrac botZ)
                                        let bot = Player
                                                { _pos = botPos
                                                , _state = botState
                                                , _team = botTeam
                                                , _distance = Just $ getDistance (deltaX, deltaY)
                                                , _visible = botIsVisible == 1
                                                }
                                        if _state bot == 0 then
                                            return $ Just bot
                                        else return Nothing
                                    Nothing -> return Nothing
                                ) (botsPointers (playersList + 0x8) (playersList + 0x10) maxPlayers)
                            let bots    = catMaybes mBots
                                mTarget = closestBot $ filter (\bot -> _team bot /= _team localPlayer && _visible bot) bots
                            case mTarget of
                                Just target -> do
                                    let (aimX, aimY) = getAngles (_pos localPlayer) (_pos target)
                                    Mem.writeFloat pid playerAimXAddress aimX
                                    Mem.writeFloat pid playerAimYAddress aimY
                                Nothing -> return ()
                            aimX <- Mem.readFloat pid playerAimXAddress
                            aimY <- Mem.readFloat pid playerAimYAddress
                            drawESP
                                localPlayer
                                (fromMaybe 0.0 aimX)
                                (fromMaybe 0.0 aimY)
                                bots
                            return ()
                        Nothing -> return ()
                Nothing -> return ()
        Nothing -> return ()
    return ()

botsPointers :: Word64 -> Word64 -> Int32 -> [Word64]
botsPointers fstAddress sndAddress maxPlayers =
    [fstAddress + (fromIntegral i * Offsets.nextBot) | i <- [0 .. ((maxPlayers `div` 2) - 1)]] ++
    [sndAddress + (fromIntegral i * Offsets.nextBot) | i <- [0 .. ((maxPlayers `div` 2) - 2)]]

getAngles :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float)
getAngles playerPos botPos = do
    let (deltaX, deltaY, deltaZ) = deltas playerPos botPos

        yawRad = atan2 deltaX (-deltaY)
        yawDeg = yawRad * 180.0 / pi
        yawDeg' = if yawDeg < 0 then yawDeg + 360.0 else yawDeg

        horizontalDistance = getDistance (deltaX, deltaY)

        pitchRad = atan2 deltaZ horizontalDistance
        pitchDeg = pitchRad * 180.0 / pi
    (yawDeg', pitchDeg)

getDistance :: (Float, Float) -> Float
getDistance (deltaX, deltaY) = sqrt ((deltaX * deltaX) + (deltaY * deltaY))

deltas :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
deltas (playerX, playerY, playerZ) (botX, botY, botZ) = (botX - playerX, botY - playerY, botZ - playerZ)

degToRad :: Float -> Float
degToRad deg = deg * pi / 180.0

normalizeDelta :: Float -> Float
normalizeDelta ang
  | ang <= -pi = normalizeDelta (ang + 2 * pi)
  | ang > pi   = normalizeDelta (ang - 2 * pi)
  | otherwise  = ang

worldToScreen :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Maybe (Float, Float, Float)
worldToScreen playerPos aimXDeg aimYDeg screenW screenH enemyPos =
    let (deltaX, deltaY, deltaZ) = deltas playerPos enemyPos
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
drawBox :: (Float, Float, Float) -> Color4 Float -> IO ()
drawBox (posX, posY, footPosY)  boxColor = do
  let boxHeight = posY - footPosY
      boxWidth = boxHeight * 0.45
      left = (posX + posX) / 2 - boxWidth / 2
      right = left + boxWidth
  color boxColor
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 left footPosY
    vertex $ Vertex2 right footPosY
    vertex $ Vertex2 right posY
    vertex $ Vertex2 left posY

drawESP :: Player -> Float -> Float -> [Player] -> IO ()
drawESP player aimX aimY bots =
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
                let (botScreenX, botScreenY, botFootScreenY) = fromMaybe (0, 0, 0) (worldToScreen (_pos player) aimX aimY (realToFrac vw) (realToFrac vh) (_pos bot))
                    boxColor = getBoxColor player bot
                drawBox (botScreenX, botScreenY, botFootScreenY) boxColor)
            bots

getBoxColor :: Player -> Player -> Color4 Float
getBoxColor player bot
    | _team player /= _team bot && not (_visible bot) = Color4 0 0 1 (1 :: Float)
    | _team player /= _team bot && _visible bot = Color4 1 0 0 (1 :: Float)
    | otherwise = Color4 0 1 0 (1 :: Float)

closestBot :: [Player] -> Maybe Player
closestBot ms =
  let candidates = [(p, d) | p <- ms, Just d <- [_distance p]]
  in if null candidates then Nothing else Just (fst (minimumBy (comparing snd) candidates))

data Player
    = Player
    { _pos      :: (Float, Float, Float)
    , _team     :: Int32
    , _state    :: Int32
    , _distance :: Maybe Float
    , _visible  :: Bool
    }
    deriving Show