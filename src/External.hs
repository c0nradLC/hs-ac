module External where

import Foreign
import Foreign.C (CBool (..), CInt (..))
import Types

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
