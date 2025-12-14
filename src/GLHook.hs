{-# LANGUAGE ForeignFunctionInterface #-}

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
import Data.List (isInfixOf, find)
import Control.Concurrent
import System.Posix.DynamicLinker
import qualified Memory as Mem
import qualified Data.ByteString as BS
import Data.Word
import GHC.Int (Int32)
import Numeric (readHex, showHex)

-- SDL2 Window type (opaque pointer)
type SDL_Window = Ptr ()

-- Our hook for SDL_GL_SwapWindow
foreign export ccall "sdlGLSwapWindowHook" sdlGLSwapWindowHook :: SDL_Window -> IO ()

sdlGLSwapWindowHook :: SDL_Window -> IO ()
sdlGLSwapWindowHook window = do

    -- Call original function
    print $ "Calling original SDL_GL_SwapWindow with window: " ++ show window
--    callSwapWindow window

    -- Draw ESP
    
    return ()

-- Function pointer callers
foreign import ccall "dynamic" 
    callSwapWindow :: FunPtr (SDL_Window -> IO ()) -> SDL_Window -> IO ()

getBotsPointers :: Word64 -> Word64 -> Int32 -> [Word64]
getBotsPointers fstAddr sndAddr maxPlayers =
    [fstAddr + fromIntegral i * nextBotOffset | i <- [0 .. maxPlayers-3]] ++
    [sndAddr + fromIntegral i * nextBotOffset | i <- [0 .. maxPlayers-4]]

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
    { _pos   :: Maybe (Float, Float, Float)
    , _team  :: Maybe Int32
    , _state :: Maybe Int32
    }
    deriving Show