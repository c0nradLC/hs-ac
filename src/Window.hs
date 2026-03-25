{-# LANGUAGE OverloadedStrings #-}

module Window (initDearImGuiWindow, drawGui) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isNothing)
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL (pollEventWithImGui, sdl2NewFrame)
import DearImGui.SDL.OpenGL
import Foreign (Ptr)
import Global (guiRef)
import SDL
import SDL.Raw.Video (glGetCurrentContext)
import Types (GuiState (GuiState, _isVisible))
import Unsafe.Coerce (unsafeCoerce)

{- Convert from the Window ptr intercepted by the SDL_GL_SwapWindow hook
   remember that SDL_GL_SwapWindow function signature is: bool SDL_GL_SwapWindow(SDL_Window *window);
   so what happens here is that we convert the window ptr to the actual SDL.Internal.Types.Window type.
-}
windowPtrToWindow :: Ptr () -> Window
windowPtrToWindow = unsafeCoerce

-- Convert from the GLContext type from SDL.Raw.Video to SDL.Video.OpenGL
glToContext :: Ptr () -> GLContext
glToContext = unsafeCoerce

-- init DearImGui with our current OpenGL context and current window, from our hooked process
initDearImGuiWindow :: Ptr () -> IO ()
initDearImGuiWindow windowPtr = do
  readIORef guiRef >>= \mbGuiState ->
    when (isNothing mbGuiState) $ do
      glCtx <- glGetCurrentContext
      _ <- createContext
      _ <- sdl2InitForOpenGL (windowPtrToWindow windowPtr) (glToContext glCtx)
      _ <- openGL3Init
      writeIORef guiRef $ Just GuiState {_isVisible = True}

drawGui :: IO ()
drawGui =
  readIORef guiRef >>= \mbGuiState -> case mbGuiState of
    Just guiState -> do
      if (_isVisible guiState)
        then unlessQuit $ do
          openGL3NewFrame
          sdl2NewFrame
          newFrame

          withWindowOpen "hackiddi hack" $ do
            text "dafuq is this on my screen!"

          render
          openGL3RenderDrawData =<< getDrawData
        else do
          writeIORef guiRef $ Just $ GuiState {_isVisible = False}
    Nothing -> return ()

-- Process the event loop
unlessQuit :: IO () -> IO ()
unlessQuit action = do
  shouldQuit <- gotQuitEvent
  if shouldQuit
    then do
      writeIORef guiRef $ Just $ GuiState {_isVisible = False}
    else action

gotQuitEvent :: IO Bool
gotQuitEvent = do
  ev <- pollEventWithImGui

  case ev of
    Nothing ->
      return False
    Just event -> do
      (isQuit event ||) <$> gotQuitEvent

isQuit :: Event -> Bool
isQuit event =
  case eventPayload event of
    KeyboardEvent kevd -> unwrapKeycode (keysymKeycode $ keyboardEventKeysym kevd) == 27 -- ESC KeyCode
    _ -> False
