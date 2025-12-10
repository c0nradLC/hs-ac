module Main where

import Data.List (find)
import qualified Memory as Mem
import Numeric (readHex, showHex)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
    mbPid <- Mem.findProcessId "linux_64_client"
    case mbPid of
        Just pid -> do
            let procPath = "/proc/" ++ show pid
                memPath = procPath ++ "/mem"
            modules <- Mem.getProcessModules $ procPath ++ "/maps"
            heapModuleBaseAddr <- case find (\mod -> Mem._name mod == "[heap]") modules of
                Just heapModule -> do
                    return $ Mem._baseAddr heapModule
                Nothing -> do
                    error "Heap module not found."
            gameModuleBaseAddr <- case find (\mod -> Mem._name mod == "linux_64_client") modules of
                Just gameModule -> do
                    return $ Mem._baseAddr gameModule
                Nothing -> do
                    error "Game/Binary module not found."
            let playerEntityAddr = heapModuleBaseAddr + (fst . head $ readHex "1d890")
            let healthAddr = playerEntityAddr + (fst . head $ readHex "100")
            let ammoAddr = playerEntityAddr + (fst . head $ readHex "154")
            let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
            -- player entity is right, probably found the pointers, just need to calculate the offset from heap to player entity
            let ammoInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fd06e")
            let recoilInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "77a9c")
            -- Need to revisit how the knockback works, it might not be this, maybe instead of NOP we need to set it to something else or might be another instruction altogether
            let knockbackInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fcf6d")


            print $ "PID: " ++ show pid

            print $ "Heap base addr: " ++ show (showHex heapModuleBaseAddr "")
            print $ "Binary base addr: " ++ show (showHex gameModuleBaseAddr "")

            putStrLn ""

            print $ "Player entity address: " ++ show (showHex playerEntityAddr "")

            putStrLn ""

            print $ "Player Aim Y axis address: " ++ show (showHex playerAimYAddr "")

            print $ "Primary ammo address: " ++ show (showHex ammoAddr "")
            print $ "Health address: " ++ show (showHex healthAddr "")
            print $ "Ammo instr address: " ++ show (showHex ammoInstrAddr "")
            print $ "Recoil instr address: " ++ show (showHex recoilInstrAddr "")
            print $ "(maybe) Knockback instr address: " ++ show (showHex knockbackInstrAddr "")

            putStrLn ""
--500e3f
            -- Set primary ammo
            Mem.writeMem memPath ammoAddr 1337
            test1 <- Mem.readInt32 pid ammoAddr
            print $ "ammoAddr val: " ++ show test1

            -- Set health
            Mem.writeMem memPath healthAddr 1337
            test2 <- Mem.readInt32 pid healthAddr
            print $ "healthAddr val: " ++ show test2

            -- Patch Infinite ammo
            ammoInstrVal <- Mem.readInstruction pid ammoInstrAddr 3
            --Mem.writeInstruction pid ammoInstrAddr (BS.pack $ replicate 3 0x90)
            -- Rollback patch
            --Mem.writeInstruction pid ammoInstrAddr $ fromMaybe (B8.pack "") ammoInstrVal

            -- No recoil
            noRecoilInstrVal <- Mem.readInstruction pid recoilInstrAddr 6
            --Mem.writeInstruction pid recoilInstrAddr (BS.pack $ replicate 6 0x90)
            -- Rollback patch
            --Mem.writeInstruction pid recoilInstrAddr $ fromMaybe (B8.pack "") noRecoilInstrVal

            -- No knockback
            --Mem.writeInstruction pid knockbackInstrAddr (BS.pack $ replicate 7 090)
            --test5 <- Mem.readInstruction pid knockbackInstrAddr 8
            --print $ "knockback val: " ++ show test5

            return ()
        Nothing -> do
            print "pid not found."
            return ()
