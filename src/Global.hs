module Global where
import Data.IORef (IORef, newIORef)
import Foreign (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)

{-# NOINLINE playerEntityPointerRef #-}
playerEntityPointerRef :: IORef Word
playerEntityPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerListPointerRef #-}
playerListPointerRef :: IORef Word
playerListPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE maxPlayersAddressRef #-}
maxPlayersAddressRef :: IORef Word
maxPlayersAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE isvisibleFunctionAddressRef #-}
isvisibleFunctionAddressRef :: IORef Word
isvisibleFunctionAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE attackFunctionAddressRef #-}
attackFunctionAddressRef :: IORef Word
attackFunctionAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerInCrosshairFunctionAddressRef #-}
playerInCrosshairFunctionAddressRef :: IORef Word
playerInCrosshairFunctionAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE dokillFunctionAddressRef #-}
dokillFunctionAddressRef :: IORef Word
dokillFunctionAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE loadedRef #-}
loadedRef :: IORef Bool
loadedRef = unsafePerformIO $ newIORef False

{-# NOINLINE originalSwapWindowFuncRef #-}
originalSwapWindowFuncRef :: IORef (Maybe (FunPtr (Ptr () -> IO ())))
originalSwapWindowFuncRef = unsafePerformIO $ newIORef Nothing