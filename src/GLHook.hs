{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import System.Posix.Process ( getProcessID)
import Data.List (minimumBy)
import qualified Memory as Mem
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, catMaybes)
import Graphics.Rendering.OpenGL (Size(..), matrixMode, loadIdentity, HasSetter (($=)), MatrixMode (Projection), ortho, HasGetter (get), viewport, lineWidth, ComparisonFunction (Always), Capability (Enabled), depthFunc, Color (color), Color4 (Color4), renderPrimitive, PrimitiveMode (LineLoop), Vertex2 (Vertex2), Vertex (vertex), preservingMatrix, BlendingFactor (SrcAlpha, OneMinusSrcAlpha), blendFunc, blend)
import Foreign.C.Types (CBool(..),)
import Memory (getGameModuleBaseAddr)
import qualified Offsets
import GHC.Conc.IO (threadDelay)
import Control.Concurrent (forkIO)
import Control.Monad (when, unless)
import System.Posix (ProcessID, dlopen, RTLDFlags (RTLD_LAZY, RTLD_GLOBAL), dlsym)
import Foreign
    ( Int32,
      Ptr,
      Storable(poke, peek), nullPtr, FunPtr, nullFunPtr, WordPtr (WordPtr), castPtrToFunPtr, wordPtrToPtr, malloc )
import Types(Player(..), ACVec(..), ACPlayer(..))
import Global
    ( playerEntityPointerRef,
      playerListPointerRef,
      maxPlayersAddressRef,
      isvisibleFunctionAddressRef,
      attackFunctionAddressRef,
      playerInCrosshairFunctionAddressRef,
      loadedRef,
      originalSwapWindowFuncRef )
import Data.IORef ( readIORef, writeIORef )

-- Our SwapWindow hook
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: Ptr () -> IO ()

-- Call to function pointer for libSDL's SDL_GL_SwapWindow
foreign import ccall "dynamic"
    callOriginalSwapWindow :: FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

-- Call to function pointer for AC's attack
foreign import ccall "dynamic"
    attack :: FunPtr (Int -> IO ()) -> Int -> IO ()

-- Call to function pointer for AC's playerincrosshair
foreign import ccall "dynamic"
    playerincrosshair :: FunPtr (IO (Ptr ACPlayer)) -> IO (Ptr ACPlayer)

-- patchClient
foreign export ccall "patchClient" patchClient :: IO ()

-- Import our bridge to call IsVisible from bot_util
foreign import ccall "isvisible" isvisible :: FunPtr (Ptr ACVec -> Ptr ACVec -> Ptr () -> CBool -> IO CBool)
                                                -> Ptr ACVec -> Ptr ACVec -> Ptr () -> CBool -> IO CBool

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
        writeIORef isvisibleFunctionAddressRef $ gameModuleBase + Offsets.isVisibleFunction
        writeIORef attackFunctionAddressRef $ gameModuleBase + Offsets.attackFunction
        writeIORef playerInCrosshairFunctionAddressRef $ gameModuleBase + Offsets.playerInCrosshairFunction

        writeIORef loadedRef True
    return ()

sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook windowPtr = do
    isLoaded <- readIORef loadedRef
    swapWindowR <- readIORef originalSwapWindowFuncRef
    let originalSwapWindow = fromMaybe nullFunPtr swapWindowR
    if originalSwapWindow == nullFunPtr then do
        dl <- dlopen "libSDL2-2.0.so" [RTLD_LAZY, RTLD_GLOBAL]
        original_SwapWindow <- dlsym dl "SDL_GL_SwapWindow"
        unless (original_SwapWindow == nullFunPtr) $ do
            writeIORef originalSwapWindowFuncRef $ Just original_SwapWindow
    else do
        when isLoaded hack
        callOriginalSwapWindow originalSwapWindow windowPtr

getLocalPlayerAndAimAddresses :: ProcessID -> IO (Player, (Word, Word))
getLocalPlayerAndAimAddresses pid = do
    playerEntityPointerAddress <- readIORef playerEntityPointerRef
    mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointerAddress
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

            return (Player
                { _pos = fromMaybe (0,0,0) mPlayerPos
                , _state = fromIntegral $ fromMaybe 4 mPlayerState
                , _team = fromIntegral $ fromMaybe 4 mPlayerTeam
                , _distance = Nothing
                , _visible = False
                , _aimX = fromMaybe 0 mPlayerAimX
                , _aimY = fromMaybe 0 mPlayerAimY
                , _baseAddr = playerEntityAddress
                }, (playerAimXAddress, playerAimYAddress))
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
                        isvisibleFunctionAddress <- readIORef isvisibleFunctionAddressRef
                        playerVecPtr <- malloc
                        botVecPtr <- malloc
                        poke playerVecPtr $ ACVec {_x = realToFrac playerX, _y = realToFrac playerY, _z = realToFrac playerZ}
                        poke botVecPtr $ ACVec {_x = realToFrac botX, _y = realToFrac botY, _z = realToFrac botZ}
                        botIsVisible <- isvisible (castPtrToFunPtr $ wordPtrToPtr $ WordPtr isvisibleFunctionAddress) playerVecPtr botVecPtr nullPtr 0
                        let bot = Player
                                { _pos = botPos
                                , _state = fromIntegral botState
                                , _team = fromIntegral botTeam
                                , _distance = Just $ getDistance (deltaX, deltaY)
                                , _visible = botIsVisible == 1 -- CBool is just an int
                                , _aimX = 0 -- We won't use a bot's aim coords for anything
                                , _aimY = 0
                                , _baseAddr = botAddress
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

    (localPlayer, playerAimAddresses)  <- getLocalPlayerAndAimAddresses pid
    playersList <- getPlayersList pid localPlayer

    -- Magnet
    playersList <- magnet pid localPlayer playersList
    -- ESP
    drawESP localPlayer (_aimX localPlayer) (_aimY localPlayer) playersList
    -- Triggerbot
    triggerBot localPlayer
    -- Aimbot
    aimbot pid playerAimAddresses localPlayer playersList

magnet :: ProcessID -> Player -> [Player] -> IO [Player]
magnet pid localPlayer players = do
    mapM (\bot -> do
        let (playerX, playerY, playerZ) = _pos localPlayer
            newPos = (playerX + 1, playerY + 1, playerZ)
        Mem.writeVec3 pid (_baseAddr bot + Offsets.playerPos) newPos
        return Player
            { _pos = (playerX + 1, playerY + 1, playerZ)
            , _team = _team bot
            , _state = _state bot
            , _distance = _distance bot
            , _visible = True
            , _aimX = _aimX bot
            , _aimY = _aimY bot
            , _baseAddr = _baseAddr bot}
        ) (filter (\bot -> _team localPlayer /= _team bot) players)

aimbot :: ProcessID -> (Word, Word) -> Player -> [Player] -> IO ()
aimbot pid (playerAimXAddress, playerAimYAddress) localPlayer playersList = do
    let mTarget = closestBot $ filter (\bot -> _team bot /= _team localPlayer && _visible bot) playersList
    case mTarget of
        Just target -> do
            let (newAimX, newAimY) = getAngles (_pos localPlayer) (_pos target)
            Mem.writeFloat pid playerAimXAddress newAimX
            Mem.writeFloat pid playerAimYAddress newAimY
        Nothing -> return ()

triggerBot :: Player -> IO ()
triggerBot localPlayer = do
    playerInCrosshairFunctionAddress <- readIORef playerInCrosshairFunctionAddressRef
    let playerInCrosshairFunPtr = castPtrToFunPtr $ wordPtrToPtr $ WordPtr playerInCrosshairFunctionAddress
    attackFunctionAddress <- readIORef attackFunctionAddressRef
    aimedAtPlayer <- do
        playerAimedAt <- playerincrosshair playerInCrosshairFunPtr 
        peek playerAimedAt
    when (_cpTeam aimedAtPlayer /= -1 && _cpTeam aimedAtPlayer /= fromIntegral (_team localPlayer)) $ do
        -- We put this on a thread and call attack with bot 1 and 0 to enable the player to shoot automatically by holding down m1 if it wants to
        _ <- forkIO $ do
            let attackFunPtr = castPtrToFunPtr $ wordPtrToPtr $ WordPtr attackFunctionAddress
            attack attackFunPtr 1
            threadDelay 1
            attack attackFunPtr 0
        return ()

botsPointers :: Word -> Word -> Int32 -> [Word]
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