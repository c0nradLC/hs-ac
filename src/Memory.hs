module Memory (writeFloat, writeInt, writeWord32, writeMemoryBytes, readFloat, readMemoryValue, readInt32, readVec3, readAddress, findProcessId, getProcessModules, Module (..))
where

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Text (pack, split, unpack)
import Data.Word (Word64, Word32, Word8)
import Foreign (
    Int32,
    Storable (peek, poke, sizeOf),
    allocaBytes,
    castPtr,
 )
import GHC.IO.Handle (SeekMode (AbsoluteSeek), hClose, hSeek)
import GHC.IO.Handle.FD (withBinaryFile)
import GHC.IO.IOMode (IOMode (ReadMode, WriteMode))
import Numeric (readHex)
import System.Directory (doesFileExist, getDirectoryContents)

writeFloat :: Int -> Word64 -> Float -> IO ()
writeFloat = writeMemoryValue

writeInt :: Int -> Word64 -> Int -> IO ()
writeInt = writeMemoryValue

writeWord32 :: Int -> Word64 -> Word32 -> IO ()
writeWord32 = writeMemoryValue

writeMemoryValue :: (Storable a) => Int -> Word64 -> a -> IO ()
writeMemoryValue pid address val = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    bytes <- allocaBytes (sizeOf val) $ \ptr -> do
        poke ptr val
        BS.packCStringLen (castPtr ptr, sizeOf val)

    withBinaryFile memPath WriteMode (\handle -> do
        hSeek handle AbsoluteSeek (fromIntegral address)
        BS.hPut handle bytes
        )

writeMemoryBytes :: Int -> Word64 -> [Word8] -> IO ()
writeMemoryBytes pid address bytes = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    let byteString = BS.pack bytes

    withBinaryFile memPath WriteMode (\handle -> do
        hSeek handle AbsoluteSeek (fromIntegral address)
        BS.hPut handle byteString
        )

-- Read a specific type from memory
readMemoryValue :: (Storable a) => Int -> Word64 -> IO (Maybe a)
readMemoryValue pid address = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    --mbBytes <- readProcessMemory pid address (sizeOf (undefined :: Word64))
    mbBytes <- withBinaryFile memPath ReadMode (\handle -> do
        let size = sizeOf (undefined :: Word64)
        hSeek handle AbsoluteSeek (fromIntegral address)
        content <- BS.hGet handle size
        hClose handle
        return $
            if BS.length content == size
                then Just content
                else Nothing
        )
    case mbBytes of
        Just bytes ->
            if BS.length bytes == sizeOf (undefined :: Word64)
                then do
                    Just <$> byteStringToValue bytes
                else return Nothing
        Nothing -> return Nothing
    where byteStringToValue bs =
            BS.useAsCString bs $ \cstr ->
            peek (castPtr cstr)

-- Read an Int32 value
readInt32 :: Int -> Word64 -> IO (Maybe Int32)
readInt32 = readMemoryValue

-- Read a float value
readFloat :: Int -> Word64 -> IO (Maybe Float)
readFloat = readMemoryValue

-- Read an Adress (hex) value
readAddress :: Int -> Word64 -> IO (Maybe Word64)
readAddress = readMemoryValue

-- Read three floats in sequence, representing an x, y, z position
readVec3 :: Int -> Word64 -> IO (Maybe (Float, Float, Float))
readVec3 pid addr = do
    mbX <- readFloat pid addr
    mbY <- readFloat pid (addr + 4)
    mbZ <- readFloat pid (addr + 8)
    case (mbX, mbY, mbZ) of
        (Just x, Just y, Just z) -> return $ Just (x, y, z)
        _ -> return Nothing

-- Find PID by name
findProcessId :: String -> IO (Maybe Int)
findProcessId processName = do
    processes <- getDirectoryContents "/proc"
    let pids = [read pid | pid <- processes, all (`elem` "0123456789") pid]

    findM isTargetProcess pids
  where
    isTargetProcess pid = do
        let commPath = "/proc/" ++ show pid ++ "/comm"
        exists <- doesFileExist commPath
        if exists
            then do
                content <- readFile commPath
                return $ processName `isInfixOf` content
            else return False


-- remove what's below this line and move it to main, it's only called one time, no real need to have it as a function

getProcessModules :: FilePath -> IO [Module]
getProcessModules fp = do
    mapsContent <- readFile fp
    return $ map getModule $ filter mapLineFilter (lines mapsContent)

getModule :: String -> Module
getModule line = do
    let address = fst . head $ readHex (unpack $ head (split (== '-') (pack (head $ words line)))) :: Word64
    Module{_name = unpack $ last (split (== '/') (pack $ last $ words line)), _baseAddr = address}

mapLineFilter :: String -> Bool
mapLineFilter mapLine =
    (length (words mapLine) == 6)
        && ( do
                let columns = words mapLine
                    file = last columns
                (file `elem` ["[stack]", "[heap]"]) || ("x" `isInfixOf` (columns !! 1))
           )

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM predicate (x : xs) = do
    result <- predicate x
    if result then return (Just x) else findM predicate xs

data Module = Module
    { _name :: String
    , _baseAddr :: Word64
    }
    deriving (Show, Eq)
