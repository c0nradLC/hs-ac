{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLHook where

import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text, pack, split, toLower)
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
import Geom (deltas, getAngles, getDistance, worldToScreen)
import Global
  ( aimbotRef,
    attackFunPtrRef,
    dokillFunPtrRef,
    espRef,
    gameModeAddressRef,
    godModeRef,
    infiniteammoRef,
    isVisibleFunPtrRef,
    loadedRef,
    magnetRef,
    maxPlayersAddressRef,
    noattackphysicsRef,
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
import qualified Offsets
import System.Environment (lookupEnv)
import System.Posix (ProcessID, RTLDFlags (RTLD_GLOBAL, RTLD_LAZY), dlopen, dlsym)
import System.Posix.Process (getProcessID)
import Types (ACPlayer (..), ACVec (..), Player (..))
import Window (drawGui, initDearImGuiWindow)

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

-- Call to function pointer for AC's dokill
foreign import ccall "dynamic"
  dokill :: FunPtr (Ptr ACPlayer -> Ptr ACPlayer -> CBool -> CInt -> IO ()) -> Ptr ACPlayer -> Ptr ACPlayer -> CBool -> CInt -> IO ()

-- Import our bridge to call IsVisible from bot_util
foreign import ccall "isvisible"
  isvisible ::
    FunPtr (Ptr ACVec -> Ptr ACVec -> Ptr () -> CBool -> IO CBool) ->
    Ptr ACVec ->
    Ptr ACVec ->
    Ptr () ->
    CBool ->
    IO CBool

-- patchClient
foreign export ccall "patchClient" patchClient :: IO ()

parseHackModes :: [Text] -> IO ()
parseHackModes modes = do
  writeIORef infiniteammoRef ("infiniteammo" `elem` modes)
  writeIORef noattackphysicsRef ("noattackphysics" `elem` modes)
  writeIORef godModeRef ("godmode" `elem` modes)
  writeIORef magnetRef ("magnet" `elem` modes)
  writeIORef triggerbotRef ("triggerbot" `elem` modes)
  writeIORef aimbotRef ("aimbot" `elem` modes)
  writeIORef espRef ("esp" `elem` modes)
  writeIORef sightKillRef ("sight" `elem` modes)

  when ("all" `elem` modes || null modes) $ do
    writeIORef infiniteammoRef True
    writeIORef noattackphysicsRef True
    writeIORef godModeRef True
    writeIORef magnetRef True
    writeIORef triggerbotRef True
    writeIORef aimbotRef True
    writeIORef espRef True
    writeIORef sightKillRef True

-- loads all the refs (static addresses) and patches the binary, what's in here gets called only once on startup after 1 second
patchClient :: IO ()
patchClient = do
  _ <- forkIO $ do
    threadDelay 1000000 -- 1 second
    pid <- getProcessID
    gameModuleBaseAddr <- Mem.getGameModuleBaseAddr $ "/proc/" ++ show pid ++ "/maps"

    -- modes available: all, infiniteammo, noattackphysics, godmode, magnet, triggerbot, aimbot, esp, sight
    hackModes <- split (== ',') . toLower . pack . fromMaybe "all" <$> lookupEnv "GIMME"
    parseHackModes hackModes

    -- write static addresses and function ptrs to IORef
    loadRefs pid gameModuleBaseAddr

    -- Infinite ammo
    readIORef infiniteammoRef >>= \active -> when active $ Mem.writeBytes pid (gameModuleBaseAddr + Offsets.consumeAmmoInstr) (replicate 3 0x90)

    -- NoSpread, NoRecoil and NoKickback (No AttackPhysics function call when shooting)
    readIORef noattackphysicsRef >>= \active -> when active $ Mem.writeBytes pid (gameModuleBaseAddr + Offsets.attackPhysicsFunction) (replicate 1 0xc3)

    -- Code cave for god-mode, this makes the player not receive damage and also deal an absurd amount of damage
    readIORef godModeRef >>= \active -> when active $ patchGodMode pid gameModuleBaseAddr $ gameModuleBaseAddr + Offsets.playerEntityPointer

    writeIORef loadedRef True
  return ()

-- stores static addresses on IORef to be used during execution
loadRefs :: ProcessID -> Word -> IO ()
loadRefs pid gameModuleBaseAddr = do
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

{- our god-mode patch, this makes us the only ones able to deal damage(or to subtract a player's health, to be more precise)
  this patches the health subtraction instruction(0x435d1c) to jump to our code cave at 0x53a951
  inside our code cave, we store our localPlayer address on the R8 register with: movabs r8, 0x<player-heap-address>
  then we write a compare(cmp) between R15 and R8, at this point in execution the attacker's address will be in R15
  then we write a jump-not-equals(jne r8, r15) which will return execution to the next instruction after the health subtraction instruction
  (that now jumps to our code cave instead) which will be at 0x435d25.
  then we write the subtraction instruction, but instead of using the damage stored in R12d, we subtract by 0xffff0000
  (btw the victim/attackee address will be in r14 at this point in execution)
  then we write a jump back to the next instruction after the health subtraction instruction(0x435d25).
  this was the funniest feature to implement and also the one that taught me the most
 TODO: disable damage when attacking a team mate
-}
patchGodMode :: ProcessID -> Word -> Word -> IO ()
patchGodMode pid gameModuleBase playerEntityPtr = do
  -- wirte near relative jmp 0xE9 to code cave from dmg subtract
  -- 53a951 - 435d1c = 104c35 - 5 = 104c30
  let jumpOffsetAddr = (gameModuleBase + Offsets.codeCave) - (gameModuleBase + Offsets.dmgSubtract)
      jumpToBytes = Mem.wordToLittleEndian $ jumpOffsetAddr - 0x5

  -- 0xe9 = near relative jump
  -- jumpToCodeCaveBytes = [0x30, 0x4c, 0x10] = ((codeCave (0x53a95) - dmgSubtract (0x435d1c)) - 0x5) -> to LE
  Mem.writeBytes pid (gameModuleBase + Offsets.dmgSubtract) $ 0xe9 : jumpToBytes

  mLocalPlayerAddr <- Mem.readAddress pid playerEntityPtr
  case mLocalPlayerAddr of
    Just localPlayerAddr -> do
      -- 0xB8 -> mov | 0x49 REX.WB prefix for R8
      -- mov r8, localPlayerAddr
      Mem.writeBytes pid (gameModuleBase + Offsets.codeCave) $ [0x49, 0xb8] ++ Mem.wordToLittleEndian localPlayerAddr
    Nothing -> return ()

  -- we will compare the address of the attacker with the address of the player
  -- at this point in time/memory, the attacker address will be on R15 while the player on R8 (we put it there in the previous instruction)
  -- if we are the ones attacking, then subtract, otherwise don't
  Mem.writeBytes pid ((gameModuleBase + Offsets.codeCave) + 0xa) [0x4d, 0x3b, 0xc7]

  -- if the attacker's address on R15 is not the same as the player(on R8), don't subtract health and go back to execution flow at 435d25
  -- to go backward we subtract our jump offset from 0x100000000.
  -- write jne back to 435d25(next instruction after our jump to code cave patch on 435d1c)
  Mem.writeBytes pid ((gameModuleBase + Offsets.codeCave) + 0xd) $ [0x0f, 0x85] ++ Mem.wordToLittleEndian ((0x100000000 - (jumpOffsetAddr + 0x5)) - 0x5)

  -- write dmg health subtract with an absurd amount inside code cave
  -- 0xffff0000 = 4294901760 if my math is correct, aint no one surviving that
  Mem.writeBytes pid ((gameModuleBase + Offsets.codeCave) + 0x13) [0x41, 0x81, 0xae, 0x0, 0x01, 0x0, 0x0, 0xff, 0xff, 0x0, 0x0]

  -- write jmp back to 435d25 to resume execution
  Mem.writeBytes pid ((gameModuleBase + Offsets.codeCave) + 0x1e) $ 0xe9 : Mem.wordToLittleEndian ((0x100000000 - (jumpOffsetAddr + 0x15)) - 0x5)

{- our actual hook for SDL_GL_SwapWindow implementatino that gets called by our C wrapper
  on startup, it obtains the address for the SDL_GL_SwapWindow symbol from the loaded libraries
  (libSDL2-2.0.so in this case) and stores it in IORef.
  after it is loaded into its IORef, checks if patchClient has already been called (by checking isLoaded)
  if it has already been patched/loaded then calls our hack funtions and then calls the original
  SDL_GL_SwapWindow that we stored in IORef.
-}
sdlGLSwapWindowHook :: Ptr () -> IO ()
sdlGLSwapWindowHook windowPtr = do
  initDearImGuiWindow windowPtr
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
      isLoaded <- readIORef loadedRef
      when isLoaded hack
      drawGui
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

{- reads the player ptr obtained by calling playerincrosshair and checks if the target player belongs to a different team
   if true then calls dokill, otherwise do nothing
-}
sightKill :: Player -> IO ()
sightKill localPlayer = do
  playerAimedAt <- readIORef playerInCrosshairPtrRef >>= peek

  -- _cpTeam comes as -1 when no player is in crosshair, we need to make sure we're passing valid ptrs to dokill otherwise the game crashes
  when (_cpTeam playerAimedAt /= -1 && fromIntegral (_cpTeam playerAimedAt) /= _team localPlayer) $ do
    readIORef dokillFunPtrRef
      >>= \dokillFunPtr ->
        readIORef playerInCrosshairPtrRef
          >>= \playerAimedAtPtr -> dokill dokillFunPtr playerAimedAtPtr (wordPtrToPtr $ WordPtr (_baseAddr localPlayer)) 1 0

-- TODO: make bots appear on player's crosshair
-- TODO: fix bug that makes player unable to hold down m1 to shoot automatically when this is enabled
-- maps the player list and updates the position of enemy players/bots to be equal to the players position with a 1 unit difference
magnet :: ProcessID -> Player -> [Player] -> IO [Player]
magnet pid localPlayer players = do
  mapM
    ( \bot -> do
        if _team bot == _team localPlayer
          then return bot
          else do
            let (playerX, playerY, playerZ) = _pos localPlayer
                newPos = (playerX + 1, playerY + 1, playerZ)
            Mem.writeVec3 pid (_baseAddr bot + Offsets.playerPos) newPos
            return
              Player
                { _pos = newPos,
                  _team = _team bot,
                  _state = _state bot,
                  _distance = _distance bot,
                  _visible = True,
                  _aimX = _aimX bot,
                  _aimY = _aimY bot,
                  _baseAddr = _baseAddr bot
                }
    )
    players

{- given the players list, gets which player is visible and closest to the player based on its _distance value,
  then obtains the new aim coordinates to place the player's crosshair(aimX and aimY) onto the targets position
  the calculation is not based on the player's current crosshair position(aimX and aimY), but instead in its own
  position in the "world"(_pos -> 3d vector)
-}
aimbot :: ProcessID -> Player -> [Player] -> IO ()
aimbot pid localPlayer playersList = do
  -- get the closest visible enemy bot as the target
  let mTarget = closestBot $ filter (\bot -> _team bot /= _team localPlayer && _visible bot) playersList
  case mTarget of
    Just target -> do
      let (newAimX, newAimY) = getAngles (_pos localPlayer) (_pos target)
      readIORef playerAimXAddressRef
        >>= \aimXAddress -> Mem.writeFloat pid aimXAddress newAimX
      readIORef playerAimYAddressRef
        >>= \aimYAddress -> Mem.writeFloat pid aimYAddress newAimY
    Nothing -> return ()

{- reads the player ptr obtained by calling playerincrosshair and checks if the target player belongs to a different team
  if true then shoots once, otherwise do nothing
  -}
triggerBot :: Player -> IO ()
triggerBot localPlayer = do
  aimedAtPlayer <- readIORef playerInCrosshairPtrRef >>= peek
  when (_cpTeam aimedAtPlayer /= -1 && _cpTeam aimedAtPlayer /= fromIntegral (_team localPlayer)) $ do
    -- We put this on a thread and call attack with both 1 and 0 to enable the player to shoot automatically by holding down m1 if it wants to
    _ <- forkIO $ do
      readIORef attackFunPtrRef >>= \attackFunPtr -> do
        attack attackFunPtr 1
        threadDelay 1000
        attack attackFunPtr 0
    return ()

{- In the player entity list, each player address pointer is 0x10 bytes apart, but they have different starting points,
  this is why we pass fstAddress and sndAddress, each starting point is 0x2 bytes apart from each other but both "step"
  in 0x10 bytes, it's like if they where two separate lists with the same step but different starting points
-}
botsPointers :: Word -> Word -> Int32 -> [Word]
botsPointers fstAddress sndAddress maxPlayers =
  [fstAddress + (fromIntegral i * Offsets.nextBot) | i <- [0 .. ((maxPlayers `div` 2) - 1)]]
    ++ [sndAddress + (fromIntegral i * Offsets.nextBot) | i <- [0 .. ((maxPlayers `div` 2) - 2)]]

-- draws the box on each player
drawBox :: (Float, Float, Float) -> Color4 Float -> IO ()
drawBox (posX, posY, footPosY) entColor = do
  -- calculates the width and height for each vertex of the box
  let boxHeight = posY - footPosY
      boxWidth = boxHeight * 0.45
      left = (posX + posX) / 2 - boxWidth / 2
      right = left + boxWidth

  -- sets the color of our drawing
  color entColor

  -- renders the box
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 left footPosY
    vertex $ Vertex2 right footPosY
    vertex $ Vertex2 right posY
    vertex $ Vertex2 left posY

{- renders the ESP layer on the window with OpenGL
  different from other tutorials I've found in the internet, we don't use the view matrix (I wasn't able to find it when debugging, embarassing)
  that's why we pass the player's aimX and aimY values and calculate the worldToScreen of the players based on that(aimX and aimY)
-}
drawESP :: Player -> Float -> Float -> [Player] -> IO ()
drawESP player aimX aimY bots =
  preservingMatrix $ do
    -- Get current viewport (Position/Size)
    vp <- get viewport
    let (_, Size vw vh) = vp -- Ignore Position x/y (0,0)

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
    mapM_
      ( \bot -> do
          let (botScreenX, botScreenY, botFootScreenY) =
                fromMaybe (0, 0, 0) (worldToScreen (_pos player) aimX aimY (realToFrac vw) (realToFrac vh) (_pos bot))
          drawBox (botScreenX, botScreenY, botFootScreenY) (boxColor player bot)
      )
      bots

{- FTGL ttf test, do be worked on later
drawSettings :: IO ()
drawSettings = do
  font <- createTextureFont "/usr/share/fonts/TTF/DejaVuSans.ttf"
  _ <- setFontFaceSize font 24 72
  renderFont font "Hello world!" Graphics.Rendering.FTGL.All
  destroyFont font
  -}

-- the color of the ESP box for each player/bot
boxColor :: Player -> Player -> Color4 Float
boxColor player bot
  | _team player /= _team bot && not (_visible bot) = Color4 0 0 1 (1 :: Float) -- Blue, enemy not visible
  | _team player /= _team bot && _visible bot = Color4 1 0 0 (1 :: Float) -- Red, enemy visible
  | otherwise = Color4 0 1 0 (1 :: Float) -- Green, team mate

-- give a list of Player, gets the one with the lowest _distance val
closestBot :: [Player] -> Maybe Player
closestBot ms =
  let candidates = [(p, d) | p <- ms, Just d <- [_distance p]]
   in if null candidates then Nothing else Just (fst (minimumBy (comparing snd) candidates))

isDeathMatchGameMode :: Int32 -> Bool
isDeathMatchGameMode gameMode = gameMode == 8
