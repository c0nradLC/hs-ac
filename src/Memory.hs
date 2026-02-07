module Memory (writeFloat, writeInt, writeWord32, writeVec3, writeBytes, readFloat, readMemoryValue, readInt32, readVec3, readAddress, wordToLittleEndian, getGameModuleBaseAddr, Module (..))
where

import Data.Bits (Bits (shiftR, (.&.)), FiniteBits (finiteBitSize))
import qualified Data.ByteString as BS
import Data.List (find, isInfixOf, unfoldr)
import Data.Text (pack, split, unpack)
import Data.Word (Word32, Word8)
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
import System.Posix (ProcessID)

writeFloat :: ProcessID -> Word -> Float -> IO ()
writeFloat = writeMemoryValue

writeInt :: ProcessID -> Word -> Int -> IO ()
writeInt = writeMemoryValue

writeWord32 :: ProcessID -> Word -> Word32 -> IO ()
writeWord32 = writeMemoryValue

writeMemoryValue :: (Storable a) => ProcessID -> Word -> a -> IO ()
writeMemoryValue pid address val = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    bytes <- allocaBytes (sizeOf val) $ \ptr -> do
        poke ptr val
        BS.packCStringLen (castPtr ptr, sizeOf val)

    withBinaryFile
        memPath
        WriteMode
        ( \handle -> do
            hSeek handle AbsoluteSeek (fromIntegral address)
            BS.hPut handle bytes
        )

writeBytes :: ProcessID -> Word -> [Word8] -> IO ()
writeBytes pid address bytes = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    let byteString = BS.pack bytes

    withBinaryFile
        memPath
        WriteMode
        ( \handle -> do
            hSeek handle AbsoluteSeek (fromIntegral address)
            BS.hPut handle byteString
        )

writeVec3 :: ProcessID -> Word -> (Float, Float, Float) -> IO ()
writeVec3 pid addr (x, y, z) = do
    writeFloat pid addr x
    writeFloat pid (addr + 4) y
    writeFloat pid (addr + 8) z

-- Read a specific type from memory
readMemoryValue :: (Storable a) => ProcessID -> Word -> IO (Maybe a)
readMemoryValue pid address = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    mbBytes <-
        withBinaryFile
            memPath
            ReadMode
            ( \handle -> do
                let size = sizeOf (undefined :: Word)
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
            if BS.length bytes == sizeOf (undefined :: Word)
                then do
                    Just <$> byteStringToValue bytes
                else return Nothing
        Nothing -> return Nothing
  where
    byteStringToValue bs =
        BS.useAsCString bs $ \cstr ->
            peek (castPtr cstr)

-- Read an Int32 value
readInt32 :: ProcessID -> Word -> IO (Maybe Int32)
readInt32 = readMemoryValue

-- Read a float value
readFloat :: ProcessID -> Word -> IO (Maybe Float)
readFloat = readMemoryValue

-- Read an Adress (hex) value
readAddress :: ProcessID -> Word -> IO (Maybe Word)
readAddress = readMemoryValue

-- Read three floats in sequence, representing an x, y, z position
readVec3 :: ProcessID -> Word -> IO (Maybe (Float, Float, Float))
readVec3 pid addr = do
    mbX <- readFloat pid addr
    mbY <- readFloat pid (addr + 4)
    mbZ <- readFloat pid (addr + 8)
    case (mbX, mbY, mbZ) of
        (Just x, Just y, Just z) -> return $ Just (x, y, z)
        _ -> return Nothing

wordToLittleEndian :: (FiniteBits a, Integral a) => a -> [Word8]
wordToLittleEndian w =
    unfoldr go 0
  where
    totalBytes = finiteBitSize w `div` 8
    go i
        | i < totalBytes =
            let shiftAmount = i * 8
                byte = fromIntegral $ (w `shiftR` shiftAmount) .&. 0xFF
             in Just (byte, i + 1)
        | otherwise = Nothing

getGameModuleBaseAddr :: FilePath -> IO Word
getGameModuleBaseAddr fp = do
    modules <- getProcessModules fp
    case find (\modl -> _name modl == "linux_64_client") modules of
        Just gameModule -> do
            return $ _baseAddr gameModule
        Nothing -> do
            error "Game/Binary module not found."

getProcessModules :: FilePath -> IO [Module]
getProcessModules fp = do
    mapsContent <- readFile fp
    return $ map getModule $ filter mapLineFilter (lines mapsContent)

getModule :: String -> Module
getModule line = do
    let address = fst . head $ readHex (unpack $ head (split (== '-') (pack (head $ words line)))) :: Word
    Module{_name = unpack $ last (split (== '/') (pack $ last $ words line)), _baseAddr = address}

mapLineFilter :: String -> Bool
mapLineFilter mapLine =
    (length (words mapLine) == 6)
        && ( do
                let columns = words mapLine
                    file = last columns
                (file `elem` ["[stack]", "[heap]"]) || ("x" `isInfixOf` (columns !! 1))
           )

data Module = Module
    { _name :: String
    , _baseAddr :: Word
    }
    deriving (Show, Eq)
