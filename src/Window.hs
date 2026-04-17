{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Window (initDearImGuiWindow, drawGui) where

import Cheat (patchGodMode)
import Control.Monad (liftM, liftM2, void, when)
import Data.IORef (atomicWriteIORef, newIORef, readIORef, writeIORef)
import DearImGui
import DearImGui.OpenGL3
import DearImGui.Raw.DrawList (addCircleFilled)
import DearImGui.SDL (pollEventWithImGui, pollEventsWithImGui, sdl2NewFrame)
import DearImGui.SDL.OpenGL
import Foreign (Bits (xor), Ptr, Storable (peek, poke), alloca, nullPtr)
import Global (aimbotRef, espRef, godModePatchedRef, godModeRef, infiniteammoPatchedRef, infiniteammoRef, magnetRef, noattackphysicsPatchedRef, noattackphysicsRef, originalDmgSubtractBytesRef, originalInfiniteAmmoBytesRef, originalNoattackphysicsBytesRef, sightKillRef, triggerbotRef)
import qualified Memory as Mem
import qualified Offsets
import SDL
import qualified SDL.Raw as SDLRaw
import SDL.Raw.Video (glGetCurrentContext)
import System.Posix (getProcessID)
import Types (ImGuiRefs (_isInitialized, _isVisible))
import Unsafe.Coerce (unsafeCoerce)

{- Convert from the Window ptr intercepted by the SDL_GL_SwapWindow hook.
   Remember that SDL_GL_SwapWindow function signature is: bool SDL_GL_SwapWindow(SDL_Window *window);
   so what happens here is that we convert the window ptr to the actual SDL.Internal.Types.Window type.
-}
windowPtrToWindow :: Ptr () -> Window
windowPtrToWindow = unsafeCoerce

-- Convert from the GLContext type from SDL.Raw.Video to SDL.Video.OpenGL
glToContext :: Ptr () -> GLContext
glToContext = unsafeCoerce

-- init DearImGui with our current OpenGL context and current window, from our hooked process
initDearImGuiWindow :: ImGuiRefs -> Ptr () -> IO ()
initDearImGuiWindow guiRefs windowPtr = do
  glCtx <- glGetCurrentContext
  _ <- createContext
  _ <- sdl2InitForOpenGL (windowPtrToWindow windowPtr) (glToContext glCtx)
  _ <- openGL3Init
  writeIORef (_isInitialized guiRefs) True

{- actual logic for our cheat window, if it's closed then check for SDLK_INSERT key event/press
  otherwise draw the window and check for SDLK_ESCAPE key event/press
-}
drawGui :: ImGuiRefs -> IO ()
drawGui guiRefs =
  readIORef (_isVisible guiRefs) >>= \isVisible ->
    if isVisible
      then do
        unlessQuit guiRefs $ do
          -- TODO: find a better way of making the events exclusive to dear-imgui
          _ <- pollEventsWithImGui

          openGL3NewFrame
          sdl2NewFrame
          newFrame

          -- window size
          windowSizeRef <- newIORef $ ImVec2 300 200
          setNextWindowSize windowSizeRef (ImGuiCond 0)

          pid <- getProcessID
          gameModuleBaseAddr <- Mem.getGameModuleBaseAddr $ "/proc/" ++ show pid ++ "/maps"

          -- actual cheat window
          withCloseableWindow "hackiddi hack" (_isVisible guiRefs) $ do
            text "dafuq is this on my screen!"
            _ <- checkbox "Magnet" magnetRef
            _ <- checkbox "Triggerbot" triggerbotRef
            _ <- checkbox "Aimbot" aimbotRef
            _ <- checkbox "ESP" espRef
            _ <- checkbox "Sight-kill" sightKillRef
            checkbox "Infinite ammo" infiniteammoRef >>= \changed -> when changed $ do
              enabled <- readIORef infiniteammoRef
              patched <- readIORef infiniteammoPatchedRef
              if enabled && not patched
                then do
                  Mem.writeBytes pid (gameModuleBaseAddr + Offsets.consumeAmmoInstr) (replicate 3 0x90) >> writeIORef infiniteammoPatchedRef True
                else readIORef originalInfiniteAmmoBytesRef >>= Mem.writeBytes pid (gameModuleBaseAddr + Offsets.consumeAmmoInstr) >> writeIORef infiniteammoPatchedRef False
            checkbox "No attack physics" noattackphysicsRef >>= \changed -> when changed $ do
              enabled <- readIORef noattackphysicsRef
              patched <- readIORef noattackphysicsPatchedRef
              if enabled && not patched
                then do
                  Mem.writeBytes pid (gameModuleBaseAddr + Offsets.attackPhysicsFunction) [0xc3] >> writeIORef noattackphysicsPatchedRef True
                else readIORef originalNoattackphysicsBytesRef >>= Mem.writeBytes pid (gameModuleBaseAddr + Offsets.attackPhysicsFunction) >> writeIORef noattackphysicsPatchedRef False
            checkbox "God mode" godModeRef >>= \changed -> when changed $ do
              enabled <- readIORef godModeRef
              patched <- readIORef godModePatchedRef
              if enabled && not patched
                then do
                  writeIORef godModePatchedRef True >> patchGodMode pid gameModuleBaseAddr (gameModuleBaseAddr + Offsets.playerEntityPointer)
                else writeIORef godModePatchedRef False >> readIORef originalDmgSubtractBytesRef >>= Mem.writeBytes pid (gameModuleBaseAddr + Offsets.dmgSubtract)

            drawList <- getForegroundDrawList
            mousePosPtr <- alloca $ \ptr -> do
              getMousePos >>= poke ptr
              return ptr

            -- our fancy mouse pointer/cursor
            addCircleFilled drawList mousePosPtr 8.0 0xFFF11FFF 32

          render
          openGL3RenderDrawData =<< getDrawData
      else do gotOpenEvent guiRefs

gotOpenEvent :: ImGuiRefs -> IO ()
gotOpenEvent guiRefs = do
  pumpEvents
  events <- SDLRaw.peepEvents nullPtr 1 SDLRaw.SDL_PEEKEVENT SDLRaw.SDL_FIRSTEVENT SDLRaw.SDL_LASTEVENT
  when (events > 0) $ do
    alloca $ \eventPtr -> do
      _ <- SDLRaw.peepEvents eventPtr 1 SDLRaw.SDL_GETEVENT SDLRaw.SDL_FIRSTEVENT SDLRaw.SDL_LASTEVENT

      ev <- peek eventPtr
      case ev of
        SDLRaw.KeyboardEvent _ _ _ _ _ kev ->
          if SDLRaw.keysymKeycode kev == SDLRaw.SDLK_INSERT
            then do
              writeIORef (_isVisible guiRefs) True
            else void $ SDLRaw.pushEvent eventPtr
        -- push the event back to the head of the event queue so AC can handle it
        _ -> void $ SDLRaw.pushEvent eventPtr

unlessQuit :: ImGuiRefs -> IO () -> IO ()
unlessQuit guiRefs action = do
  shouldQuit <- gotQuitEvent
  if shouldQuit
    then do
      writeIORef (_isVisible guiRefs) False
    else action

gotQuitEvent :: IO Bool
gotQuitEvent = do
  ev <- pollEventWithImGui

  case ev of
    Nothing ->
      return False
    Just event -> return $ isQuit event

isQuit :: Event -> Bool
isQuit event =
  case eventPayload event of
    KeyboardEvent kevd -> unwrapKeycode (keysymKeycode $ keyboardEventKeysym kevd) == SDLRaw.SDLK_ESCAPE
    _ -> False
