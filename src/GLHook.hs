{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Maybe (fromMaybe, catMaybes, isJust, fromJust)
import Graphics.Rendering.OpenGL (Size(..), matrixMode, loadIdentity, HasSetter (($=)), MatrixMode (Modelview, Projection), ortho, HasGetter (get), viewport, GLdouble, lineWidth, depthMask, ComparisonFunction (Less, Always), Capability (Enabled, Disabled), depthFunc, Color (color), Color4 (Color4), renderPrimitive, PrimitiveMode (LineLoop), Vertex2 (Vertex2), Vertex (vertex), preservingMatrix, BlendingFactor (SrcAlpha, OneMinusSrcAlpha), blendFunc, GLfloat, TextureFunction (Blend), blend)

-- SDL2 Window type (opaque pointer)
type SDL_Window = Ptr ()

-- Our hook for SDL_SwapWindow
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
            let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
            let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
            let playerAimXAddr = playerEntityAddr + (fst . head $ readHex "38")

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
            mPlayerPos <- Mem.readVec3 pid playerPosAddr
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

degToRad :: Float -> Float
degToRad deg = deg * pi / 180.0

normalizeDelta :: Float -> Float
normalizeDelta ang
  | ang <= -pi = normalizeDelta (ang + 2 * pi)
  | ang > pi   = normalizeDelta (ang - 2 * pi)
  | otherwise  = ang

worldToScreen :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Maybe (Float, Float)
worldToScreen camPos camYawDeg camPitchDeg screenW screenH enemyPos =
  let (deltaX, deltaY, deltaZ) = getDeltas camPos enemyPos
      horizDist = getDistance (deltaX, deltaY)
  in if horizDist < 0.01
     then Nothing
     else let yawToRad = atan2 deltaX (-deltaY)
              pitchToRad = atan2 deltaZ horizDist
              camYawRad = degToRad camYawDeg
              camPitchRad = degToRad camPitchDeg
              deltaYaw = normalizeDelta (yawToRad - camYawRad)
              deltaPitch = normalizeDelta (pitchToRad - camPitchRad)
              dotProd = cos deltaYaw * cos deltaPitch
          in if dotProd < 0 || abs deltaYaw > pi / 1.8 || abs deltaPitch > pi / 1.8
             then Nothing
             else let halfHfovRad = degToRad (90 / 2)
                      aspectRatio = screenH / screenW
                      halfVfovRad = atan (tan halfHfovRad * aspectRatio)
                      screenX = screenW / 2 + (screenW / 2) * (tan deltaYaw / tan halfHfovRad)
                      screenY = screenH / 2 - (screenH / 2) * (tan deltaPitch / tan halfVfovRad)
                  in if screenX < -100 || screenX > screenW + 100 || screenY < -100 || screenY > screenH + 100
                     then Nothing
                     else Just (screenX, screenY)

-- Draw box on bot
drawEnemyBox :: (Float, Float) -> (Float, Float) -> IO ()
drawEnemyBox foot head = do
  let fx = fst foot
      fy = snd foot
      hx = fst head
      hy = snd head
      boxHeight = abs (hy - fy)
      boxWidth = boxHeight * 0.45
      left = (fx + hx) / 2 - boxWidth / 2
      right = left + boxWidth
      bottom = fy
      top = hy
  color $ Color4 1 0 0 (0.8 :: Float)
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 left bottom
    vertex $ Vertex2 right bottom
    vertex $ Vertex2 right top
    vertex $ Vertex2 left top

-- Draw box on bot
drawEnemyBoxNew :: (Float, Float) -> IO ()
drawEnemyBoxNew pos = do
  let fx = fst pos
      fy = snd pos
      hx = fst pos
      hy = snd pos
      boxHeight = abs (hy - fy)
      boxWidth = boxHeight * 0.45
      left = (fx + hx) / 2 - boxWidth / 2
      right = left + boxWidth
      bottom = fy
      top = hy
  color $ Color4 1 0 0 (0.8 :: Float)
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 left bottom
    vertex $ Vertex2 right bottom
    vertex $ Vertex2 right top
    vertex $ Vertex2 left top

drawESP :: (Float, Float, Float) -> Float -> Float -> [Player] -> IO ()
drawESP camPos camYaw camPitch enemies =
    preservingMatrix $ do

    -- Get current viewport (Position/Size)
    vp <- get viewport
    let (_, Size vw vh) = vp  -- Ignore Position x/y (0,0)
        projectedEnemies = do
            enemy <- enemies
            let mfoot = worldToScreen camPos camYaw camPitch (realToFrac vw) (realToFrac vh) (_pos enemy)
                (enemyX, enemyY, enemyZ) = _pos enemy
                mhead = worldToScreen camPos camYaw camPitch (realToFrac vw) (realToFrac vh) (enemyX, enemyY, enemyZ - 4.5)
            guard (isJust mfoot && isJust mhead)
            let foot = fromJust mfoot
                head = fromJust mhead
            pure (foot, head)

    -- 2D overlay setup
    matrixMode $= Projection
    loadIdentity
    ortho 0 (realToFrac vw) (realToFrac vh) 0 (-1) 1

    matrixMode $= Modelview 0
    loadIdentity

    -- Overlay states
    depthFunc $= Just Always
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 2.0

    -- Draw all boxes
    --mapM_ (\bot -> do
    --        let (botX, botY) = fromMaybe (0, 0) (worldToScreen camPos camYaw camPitch (realToFrac vw) (realToFrac vh) (_pos bot))
    --        drawEnemyBoxNew (botX, botY))
    --    enemies

    mapM_ (uncurry drawEnemyBox) projectedEnemies

    -- Restore states
    depthFunc $= Just Less
    blend $= Disabled
    lineWidth $= 1.0
    matrixMode $= Modelview 0

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