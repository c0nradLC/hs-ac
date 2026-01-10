{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Foreign.Ptr ( Ptr )
import System.Posix.Process ( getProcessID)
import Data.List (minimumBy)
import qualified Memory as Mem
import Data.Word ( Word64 )
import GHC.Int (Int32)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, catMaybes)
import Graphics.Rendering.OpenGL (Size(..), matrixMode, loadIdentity, HasSetter (($=)), MatrixMode (Projection), ortho, HasGetter (get), viewport, lineWidth, ComparisonFunction (Always), Capability (Enabled), depthFunc, Color (color), Color4 (Color4), renderPrimitive, PrimitiveMode (LineLoop), Vertex2 (Vertex2), Vertex (vertex), preservingMatrix, BlendingFactor (SrcAlpha, OneMinusSrcAlpha), blendFunc, blend)
import Foreign.C.Types (CFloat(..), CBool(..), CUInt(..), CInt(..))
import Memory (getGameModuleBaseAddr)
import qualified Offsets
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import GHC.IO (unsafePerformIO)
import GHC.Conc.IO (threadDelay)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import System.Posix (ProcessID)
import Numeric (showHex)

-- our SwapWindow hook
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: Ptr () -> IO ()

-- patchClient
foreign export ccall "patchClient" patchClient :: IO ()

-- Import our bridge to call IsVisible from bot_util.cpp
foreign import ccall unsafe "isvisible" isVisible :: CUInt -> CFloat -> CFloat -> CFloat
                                                -> CFloat -> CFloat -> CFloat -> IO CBool

-- Import our bridge to call attack from physics.cpp
foreign import ccall unsafe "attack" attack :: CUInt -> CBool -> IO ()

{-
    Import our bridge to call playerincrosshair from weapon.cpp
    Our implementation actually returns the team of the player in crosshair, if there are any
-}
foreign import ccall unsafe "playerincrosshair" playerincrosshair :: CUInt -> IO CInt

{-# NOINLINE playerEntityPointerRef #-}
playerEntityPointerRef :: IORef Word64
playerEntityPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerListPointerRef #-}
playerListPointerRef :: IORef Word64
playerListPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE maxPlayersAddressRef #-}
maxPlayersAddressRef :: IORef Word64
maxPlayersAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE isVisibleFunctionAddressRef #-}
isVisibleFunctionAddressRef :: IORef CUInt
isVisibleFunctionAddressRef = unsafePerformIO $ newIORef 0

{-# NOINLINE attackFunctionAddressRef #-}
attackFunctionAddressRef :: IORef CUInt
attackFunctionAddressRef = unsafePerformIO $ newIORef 0

{-# NOINLINE playerInCrosshairFunctionAddressRef #-}
playerInCrosshairFunctionAddressRef :: IORef CUInt
playerInCrosshairFunctionAddressRef = unsafePerformIO $ newIORef 0

{-# NOINLINE loadedRef #-}
loadedRef :: IORef Bool
loadedRef = unsafePerformIO $ newIORef False

patchClient :: IO ()
patchClient = do
    _ <- forkIO $ do
        threadDelay 1000000
        pid <- getProcessID
        gameModuleBase <- getGameModuleBaseAddr $ "/proc/" ++ show pid ++ "/maps"

        -- Infinite ammo
        Mem.writeMemoryBytes pid (gameModuleBase + Offsets.consumeAmmoInstr) (replicate 3 0x90)

        -- NoSpread, NoRecoil and NoKickback (No AttackPhysics function call when shooting)
        Mem.writeMemoryBytes pid (gameModuleBase + Offsets.attackPhysicsFunction) (replicate 1 0xc3)

        writeIORef playerEntityPointerRef $ gameModuleBase + Offsets.playerEntityPointer
        writeIORef playerListPointerRef $ gameModuleBase + Offsets.playerListPointer
        writeIORef maxPlayersAddressRef $ gameModuleBase + Offsets.maxPlayers
        writeIORef isVisibleFunctionAddressRef $ fromIntegral $ gameModuleBase + Offsets.isVisibleFunction
        writeIORef attackFunctionAddressRef $ fromIntegral $ gameModuleBase + Offsets.attackFunction
        writeIORef playerInCrosshairFunctionAddressRef $ fromIntegral $ gameModuleBase + Offsets.playerInCrosshairFunction

        writeIORef loadedRef True
    return ()

sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook _ = do
    isLoaded <- readIORef loadedRef
    when isLoaded hack

getLocalPlayer :: ProcessID -> IO Player
getLocalPlayer pid = do
    playerEntityPointer <- readIORef playerEntityPointerRef
    mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointer
    case mPlayerEntityAddress of
        Just playerEntityAddress -> do
            let playerHealthAddress = playerEntityAddress + Offsets.playerHealth
                playerPosAddress = playerEntityAddress + Offsets.playerPos
                playerAimYAddress = playerEntityAddress + Offsets.playerAimY
                playerAimXAddress = playerEntityAddress + Offsets.playerAimX
                playerTeamAddress = playerEntityAddress + Offsets.playerTeam
                playerStateAddress = playerEntityAddress + Offsets.playerState

            -- Set health
            Mem.writeInt pid playerHealthAddress 9999

            mPlayerState <- Mem.readInt32 pid playerStateAddress
            mPlayerPos <- Mem.readVec3 pid playerPosAddress
            mPlayerTeam <- Mem.readInt32 pid playerTeamAddress
            mPlayerAimX <- Mem.readFloat pid playerAimXAddress
            mPlayerAimY <- Mem.readFloat pid playerAimYAddress

            return Player
                { _pos = fromMaybe (0,0,0) mPlayerPos
                , _state = fromMaybe 4 mPlayerState
                , _team = fromMaybe 4 mPlayerTeam
                , _distance = Nothing
                , _visible = False
                , _aimX = Just (playerAimXAddress, fromMaybe 0 mPlayerAimX)
                , _aimY = Just (playerAimYAddress, fromMaybe 0 mPlayerAimY)
                }
        Nothing -> error "Error trying to read the Player's entity address."

getPlayersList :: ProcessID -> Player -> IO [Player]
getPlayersList pid localPlayer = do
    mMaxPlayersAddress <- readIORef maxPlayersAddressRef
    mMaxPlayers <- Mem.readInt32 pid mMaxPlayersAddress

    mPlayersListPointer <- readIORef playerListPointerRef
    mPlayersListAddress <- Mem.readAddress pid mPlayersListPointer
    case (mMaxPlayers, mPlayersListAddress) of
        (Just maxPlayers, Just playersList) -> do
            mBots <- mapM (\botPointer -> do
                mBotAddress <- Mem.readAddress pid botPointer
                case mBotAddress of
                    Just botAddress -> do
                        mBotState <- Mem.readInt32 pid (botAddress + Offsets.playerState)
                        mBotPos <- Mem.readVec3 pid (botAddress + Offsets.playerPos)
                        mBotTeam <- Mem.readInt32 pid (botAddress + Offsets.playerTeam)
                        let botState = fromMaybe 4 mBotState
                            botPos@(botX, botY, botZ) = fromMaybe (0,0,0) mBotPos
                            botTeam = fromMaybe 4 mBotTeam
                            (deltaX, deltaY, _) = deltas (_pos localPlayer) botPos
                            (playerX, playerY, playerZ) = _pos localPlayer
                        isVisibleFunctionAddress <- readIORef isVisibleFunctionAddressRef
                        botIsVisible <- isVisible isVisibleFunctionAddress (realToFrac playerX) (realToFrac playerY) (realToFrac playerZ) (realToFrac botX) (realToFrac botY) (realToFrac botZ)
                        let bot = Player
                                { _pos = botPos
                                , _state = botState
                                , _team = botTeam
                                , _distance = Just $ getDistance (deltaX, deltaY)
                                , _visible = botIsVisible == 1 -- CBool is just an int
                                , _aimX = Nothing -- We won't use a bot's aim coords for anything
                                , _aimY = Nothing
                                }
                        if _state bot == 0 then
                            return $ Just bot
                        else return Nothing
                    Nothing -> return Nothing
                ) (botsPointers (playersList + 0x8) (playersList + 0x10) maxPlayers)
            return $ catMaybes mBots
        (_, _) -> error "Error trying to read max players and players list addresses."

hack :: IO ()
hack = do
    pid <- getProcessID

    localPlayer <- getLocalPlayer pid
    playersList <- getPlayersList pid localPlayer

    -- Aimbot target
    let mTarget = closestBot $ filter (\bot -> _team bot /= _team localPlayer && _visible bot) playersList
        (playerAimXAddress, _) = fromMaybe (0x0, 0) (_aimX localPlayer)
        (playerAimYAddress, _) = fromMaybe (0x0, 0) (_aimY localPlayer)
        updatedLocalPlayer = case mTarget of
            Just target -> do
                let (newAimX, newAimY) = getAngles (_pos localPlayer) (_pos target)
                localPlayer {_aimX = Just (playerAimXAddress, newAimX), _aimY = Just (playerAimYAddress, newAimY)}
            Nothing -> localPlayer
    let (_, aimX) = fromMaybe (0x0, 0) (_aimX updatedLocalPlayer)
        (_, aimY) = fromMaybe (0x0, 0) (_aimY updatedLocalPlayer)
    Mem.writeFloat pid playerAimXAddress aimX
    Mem.writeFloat pid playerAimYAddress aimY
    -- ESP
    drawESP localPlayer aimX aimY playersList
    -- Triggerbot
    triggerBot updatedLocalPlayer

triggerBot :: Player -> IO ()
triggerBot localPlayer = do
    playerInCrosshairFunctionAddress <- readIORef playerInCrosshairFunctionAddressRef
    attackFunctionAddress <- readIORef attackFunctionAddressRef
    aimedAtTeam <- playerincrosshair playerInCrosshairFunctionAddress
    if aimedAtTeam /= -1 && aimedAtTeam /= fromIntegral (_team localPlayer) then do
        attack attackFunctionAddress 1
    else
        attack attackFunctionAddress 0

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

drawBox :: (Float, Float, Float) -> Color4 Float -> IO ()
drawBox (posX, posY, footPosY) entColor = do
  let boxHeight = posY - footPosY
      boxWidth = boxHeight * 0.45
      left = (posX + posX) / 2 - boxWidth / 2
      right = left + boxWidth
  color entColor
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
                drawBox (botScreenX, botScreenY, botFootScreenY) (boxColor player bot))
            bots

boxColor :: Player -> Player -> Color4 Float
boxColor player bot
    | _team player /= _team bot && not (_visible bot) = Color4 0 0 1 (1 :: Float) -- Blue, enemy not visible
    | _team player /= _team bot && _visible bot = Color4 1 0 0 (1 :: Float) -- Red, enemy visible
    | otherwise = Color4 0 1 0 (1 :: Float) -- Green, team mate

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
    , _aimX     :: Maybe (Word64, Float)
    , _aimY     :: Maybe (Word64, Float)
    }
    deriving Show