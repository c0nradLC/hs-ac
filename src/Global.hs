module Global where

import Data.IORef (IORef, newIORef)
import Foreign (FunPtr, nullFunPtr, nullPtr)
import Foreign.C (CBool, CInt)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Types (ACPlayer, ACVec, GuiState)

{-# NOINLINE playerEntityPointerRef #-}
playerEntityPointerRef :: IORef Word
playerEntityPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerAimXAddressRef #-}
playerAimXAddressRef :: IORef Word
playerAimXAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerAimYAddressRef #-}
playerAimYAddressRef :: IORef Word
playerAimYAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE playerListPointerRef #-}
playerListPointerRef :: IORef Word
playerListPointerRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE maxPlayersAddressRef #-}
maxPlayersAddressRef :: IORef Word
maxPlayersAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE gameModeAddressRef #-}
gameModeAddressRef :: IORef Word
gameModeAddressRef = unsafePerformIO $ newIORef 0x0

{-# NOINLINE isVisibleFunPtrRef #-}
isVisibleFunPtrRef :: IORef (FunPtr (Ptr ACVec -> Ptr ACVec -> Ptr () -> CBool -> IO CBool))
isVisibleFunPtrRef = unsafePerformIO $ newIORef nullFunPtr

{-# NOINLINE attackFunPtrRef #-}
attackFunPtrRef :: IORef (FunPtr (Int -> IO ()))
attackFunPtrRef = unsafePerformIO $ newIORef nullFunPtr

{-# NOINLINE playerInCrosshairFunPtrRef #-}
playerInCrosshairFunPtrRef :: IORef (FunPtr (IO (Ptr ACPlayer)))
playerInCrosshairFunPtrRef = unsafePerformIO $ newIORef nullFunPtr

{-# NOINLINE dokillFunPtrRef #-}
dokillFunPtrRef :: IORef (FunPtr (Ptr ACPlayer -> Ptr ACPlayer -> CBool -> CInt -> IO ()))
dokillFunPtrRef = unsafePerformIO $ newIORef nullFunPtr

-- Store the playerincrosshair ptr so we don't have to call it more than once per frame
{-# NOINLINE playerInCrosshairPtrRef #-}
playerInCrosshairPtrRef :: IORef (Ptr ACPlayer)
playerInCrosshairPtrRef = unsafePerformIO $ newIORef nullPtr

{-# NOINLINE loadedRef #-}
loadedRef :: IORef Bool
loadedRef = unsafePerformIO $ newIORef False

{-# NOINLINE originalSwapWindowFuncRef #-}
originalSwapWindowFuncRef :: IORef (Maybe (FunPtr (Ptr () -> IO ())))
originalSwapWindowFuncRef = unsafePerformIO $ newIORef Nothing

-- all the hack features/modes

{-# NOINLINE infiniteammoRef #-}
infiniteammoRef :: IORef Bool
infiniteammoRef = unsafePerformIO $ newIORef False

{-# NOINLINE noattackphysicsRef #-}
noattackphysicsRef :: IORef Bool
noattackphysicsRef = unsafePerformIO $ newIORef False

{-# NOINLINE godModeRef #-}
godModeRef :: IORef Bool
godModeRef = unsafePerformIO $ newIORef False

{-# NOINLINE magnetRef #-}
magnetRef :: IORef Bool
magnetRef = unsafePerformIO $ newIORef False

{-# NOINLINE sightKillRef #-}
sightKillRef :: IORef Bool
sightKillRef = unsafePerformIO $ newIORef False

{-# NOINLINE espRef #-}
espRef :: IORef Bool
espRef = unsafePerformIO $ newIORef False

{-# NOINLINE triggerbotRef #-}
triggerbotRef :: IORef Bool
triggerbotRef = unsafePerformIO $ newIORef False

{-# NOINLINE aimbotRef #-}
aimbotRef :: IORef Bool
aimbotRef = unsafePerformIO $ newIORef False

{-# NOINLINE guiRef #-}
guiRef :: IORef (Maybe GuiState)
guiRef = unsafePerformIO $ newIORef Nothing
