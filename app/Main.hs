module Main where

import Data.List (find)
import qualified Memory as Mem
import Numeric (readHex, showHex)

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
            let ammoAddr = heapModuleBaseAddr + (fst . head $ readHex "1d9e4")
            let healthAddr = ammoAddr - (fst . head $ readHex "54")
            let playerEntityAddr = healthAddr - (fst . head $ readHex "100")
            let ammoInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "FD06E")
            let playerPosAddr = playerEntityAddr + (fst . head $ readHex "2C")
            let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")

            print $ "PID: " ++ show pid

            print $ "Heap base addr: " ++ show (showHex heapModuleBaseAddr "")
            print $ "Binary base addr: " ++ show (showHex gameModuleBaseAddr "")

            putStrLn ""

            print $ "Player entity address: " ++ show (showHex playerEntityAddr "")
            print $ "Player pos address: " ++ show (showHex playerPosAddr "")
            print $ "Player Aim Y axis address: " ++ show (showHex playerAimYAddr "")

            print $ "Primary ammo address: " ++ show (showHex ammoAddr "")
            print $ "Health address: " ++ show (showHex healthAddr "")
            print $ "Ammo instr address: " ++ show (showHex ammoInstrAddr "")

            Mem.writeMem memPath ammoAddr 1337
            test1 <- Mem.readInt32 pid ammoAddr
            print $ "ammoAddr val: " ++ show test1

            Mem.writeMem memPath healthAddr 1337
            test2 <- Mem.readInt32 pid healthAddr
            case test2 of
                Just val -> print $ "healthAddr val: " ++ show val
                Nothing -> return ()

            --Mem.writeMemoryBytes pid ammoInstrAddr [0x90]
            test3 <- Mem.readInstruction pid ammoInstrAddr 8
            print $ "ammoInstrAddr val: " ++ show test3

            return ()
        Nothing -> do
            print "pid not found."
            return ()
