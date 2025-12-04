module Main where
import System.Directory (getDirectoryContents, doesFileExist)
import Debug.Trace (trace)
import Data.List (isInfixOf, find)
import Control.Monad (when)
import Foreign (WordPtr, Word64, Word32, allocaBytes, Storable (poke, sizeOf, peek), castPtr, copyBytes, Int32, Word8)
import Text.Printf (FormatParse(fpChar))
import Text.Read (readMaybe)
import Data.Text (unpack, split, pack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Foreign.C (newCStringLen)
import Data.ByteString (packCStringLen)
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(WriteMode, ReadMode))
import GHC.IO.Handle (hSeek, SeekMode (AbsoluteSeek), hClose, hSetBinaryMode)
import qualified Data.ByteString as BS
import GHC.IO.Handle.FD (openBinaryFile)
import Numeric (readHex, showHex)

main :: IO ()
main = do
    mbPid <- findProcessId "linux_64_client"
    case mbPid of
        Just pid -> do
            let procPath = "/proc/" ++ show pid
                memPath  = procPath ++ "/mem"
            modules <- getProcessModulesDef $ procPath ++ "/maps"
            heapModuleBaseAddr <- case find (\mod -> _name mod == "[heap]") modules of
                    Just heapModule -> do
                        return $ _baseAddr heapModule
                    Nothing -> do
                        error "Heap module not found."
            gameModuleBaseAddr <- case find (\mod -> _name mod == "linux_64_client") modules of
                    Just gameModule -> do
                        return $ _baseAddr gameModule
                    Nothing -> do
                        error "Game/Binary module not found."
            let ammoAddr = heapModuleBaseAddr + (fst. head $ readHex "1d9e4")
            let healthAddr = ammoAddr - (fst. head $ readHex "54")
            let playerAddr = healthAddr - (fst. head $ readHex "100")
            let ammoInstrAddr = gameModuleBaseAddr + (fst. head $ readHex "FD06E")

            print $ "PID: " ++ show pid

            print $ "Heap base addr: " ++ show (showHex heapModuleBaseAddr "")
            print $ "Binary base addr: " ++ show (showHex gameModuleBaseAddr "")

            print $ "Ammo address: " ++ show (showHex ammoAddr "")
            print $ "Health address: " ++ show (showHex healthAddr "")
            print $ "Player entity address: " ++ show (showHex playerAddr "")
            print $ "Ammo instr address: " ++ show (showHex ammoInstrAddr "")

            writeMem memPath ammoAddr 1000
            test1 <- readInt32 pid ammoAddr
            print $ "ammoAddr val: " ++ show test1

            writeMem memPath healthAddr 9999
            test2 <- readInt32 pid healthAddr
            print $ "healthAddr val: " ++ show test2

            writeMemoryBytes pid ammoInstrAddr [0x90]
            test3 <- readInstruction pid ammoInstrAddr 8
            print $ "ammoInstrAddr val: " ++ show test3

            return ()

        Nothing -> do
            print "pid not found."
            return ()

writeMem :: FilePath -> Word64 -> Word32 -> IO ()
writeMem memPath addr val = do
    bytes <- allocaBytes (sizeOf val) $ \ptr -> do
        poke ptr val
        BS.packCStringLen (castPtr ptr, sizeOf val)
    handle <- openBinaryFile memPath WriteMode

    hSeek handle AbsoluteSeek (fromIntegral addr)

    BS.hPut handle bytes
    hClose handle
    return ()

writeMemoryBytes :: Int -> Word64 -> [Word8] -> IO Bool
writeMemoryBytes pid address bytes = do
    let memPath = "/proc/" ++ show pid ++ "/mem"
    let byteString = BS.pack bytes

    handle <- openBinaryFile memPath WriteMode
    hSeek handle AbsoluteSeek (fromIntegral address)
    BS.hPut handle byteString
    hClose handle
    return True

-- Read from process memory via /proc/<pid>/mem
readProcessMemory :: Int -> Word64 -> Int -> IO (Maybe BS.ByteString)
readProcessMemory pid address size = do
    let memPath = "/proc/" ++ show pid ++ "/mem"

    -- Check if file exists and we can read it
    exists <- doesFileExist memPath
    if not exists
        then return Nothing
        else do
            -- Open the memory file
            handle <- openBinaryFile memPath ReadMode

            -- Seek to the address
            hSeek handle AbsoluteSeek (fromIntegral address)

            -- Read the memory
            content <- BS.hGet handle size
            hClose handle

            return $ if BS.length content == size
                        then Just content
                        else Nothing

-- Read a specific type from memory
readMemoryValue :: (Storable a) => Int -> Word64 -> IO (Maybe a)
readMemoryValue pid address = do
    mbBytes <- readProcessMemory pid address (sizeOf (undefined :: Word64))
    case mbBytes of
        Just bytes ->
            if BS.length bytes == sizeOf (undefined :: Word64)
                then do
                    Just <$> byteStringToValue bytes
                else return Nothing
        Nothing -> return Nothing

byteStringToValue :: (Storable a) => BS.ByteString -> IO a
byteStringToValue bs =
    BS.useAsCString bs $ \cstr ->
        peek (castPtr cstr)

readString :: Int -> Word64 -> Int -> IO (Maybe String)
readString pid addr maxLength = do
    mbBytes <- readProcessMemory pid addr maxLength
    case mbBytes of
        Just bytes ->
            let str = takeWhile (/= '\0') $ map (toEnum . fromEnum) $ BS.unpack bytes
            in return $ if null str then Nothing else Just str
        Nothing -> return Nothing

readInstructionBytes :: Int -> Word64 -> Int -> IO (Maybe BS.ByteString)
readInstructionBytes pid address instructionSize = do
    readProcessMemory pid address instructionSize

readInt32 :: Int -> Word64 -> IO (Maybe Int32)
readInt32 = readMemoryValue

readInstruction :: Int -> Word64 -> Int -> IO (Either String BS.ByteString)
readInstruction pid address size = do
    mbInstruction <- readInstructionBytes pid address size
    case mbInstruction of
        Just instr -> return (Right instr)
        Nothing -> return (Left $ "Failed to read instruction at address 0x" ++ showHex address "")

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


getProcessModulesDef :: FilePath -> IO [Module]
getProcessModulesDef fp = do
    mapsContent <- readFile fp
    return $ map getModule $ filter mapLineFilter (lines mapsContent)

getModule :: String -> Module
getModule line = do
    let address = fst . head $ readHex (unpack $ head (split (== '-') (pack (head $ words line)))) :: Word64
    Module {_name = unpack $ last (split (== '/') (pack $ last $ words line)), _baseAddr = address}

mapLineFilter :: String -> Bool
mapLineFilter mapLine =
    (length (words mapLine) == 6) && (do
                    let columns = words mapLine
                        file    = last columns
                    (file `elem` ["[stack]", "[heap]"]) || ("x" `isInfixOf` (columns !! 1))
                    )

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM pred (x:xs) = do
    result <- pred x
    if result then return (Just x) else findM pred xs

data Module = Module
    { _name :: String
    , _baseAddr :: Word64
    }deriving (Show, Eq)