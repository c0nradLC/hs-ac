module Main where

import Data.List (find)
import qualified Memory as Mem
import Numeric (readHex, showHex)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B8
import Data.Word
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder as B8

main :: IO ()
main = do
    mbPid <- Mem.findProcessId "linux_64_client"
    case mbPid of
        Just pid -> do
            let procPath = "/proc/" ++ show pid
                memPath = procPath ++ "/mem"
            modules <- Mem.getProcessModules $ procPath ++ "/maps"
            gameModuleBaseAddr <- case find (\mod -> Mem._name mod == "linux_64_client") modules of
                Just gameModule -> do
                    return $ Mem._baseAddr gameModule
                Nothing -> do
                    error "Game/Binary module not found."
            let playerEntityPointer = gameModuleBaseAddr + (fst . head $ readHex "19d518")
            let playerListPointer = gameModuleBaseAddr + (fst . head $ readHex "19d520")
            let maxPlayersPointer = gameModuleBaseAddr + (fst . head $ readHex "19d52C")
            let ammoInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fd06e")
            let recoilInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "77a9c")
            -- Need to revisit how the knockback works, it might not be this, maybe instead of NOP we need to patch it differently
            -- let knockbackInstrAddr = gameModuleBaseAddr + (fst . head $ readHex "fcf6d")

            mMaxPlayersAddr <- Mem.readInt32 pid maxPlayersPointer
            case mMaxPlayersAddr of
                Just maxPlayersAddr -> do
                        print $ "Max players: " ++ show (showHex maxPlayersAddr "")

                        mPlayersListAddress <- Mem.readAddress pid playerListPointer
                        case mPlayersListAddress of
                            Just playersListAddr -> do
                                let firstBot = fstBotAddress playersListAddr
                                let secondBot = sndBotAddress playersListAddr


                            Nothing -> do return ()
                Nothing -> return ()

            print $ "Player entity pointer: " ++ show (showHex playerEntityPointer "")
            print $ "Player list pointer: " ++ show (showHex playerListPointer "")

            mPlayerEntityAddress <- Mem.readAddress pid playerEntityPointer
            case mPlayerEntityAddress of
                Just playerEntityAddr -> do 
                    print $ "Player entity address: " ++ show (showHex playerEntityAddr "")
                    let healthAddr = playerEntityAddr + (fst . head $ readHex "100")
                    let ammoAddr = playerEntityAddr + (fst . head $ readHex "154")
                    let playerAimYAddr = playerEntityAddr + (fst . head $ readHex "3C")
                    let playerPosAddr = playerEntityAddr + (fst . head $ readHex "8")
                    
                    mPlayerPos <- Mem.readVec3 pid playerPosAddr
                    print $ "Player position: " ++ show mPlayerPos
                    

                    -- Set primary ammo
                    Mem.writeMem memPath ammoAddr 1337
                    test1 <- Mem.readInt32 pid ammoAddr
                    --print $ "AmmoAddr val: " ++ show test1

                    -- Set health
                    Mem.writeMem memPath healthAddr 1337
                    test2 <- Mem.readInt32 pid healthAddr
                    --print $ "HealthAddr val: " ++ show test2
                    return ()

                Nothing -> return ()

            print $ "PID: " ++ show pid

            --print $ "Binary base addr: " ++ show (showHex gameModuleBaseAddr "")

            --print $ "Ammo instr address: " ++ show (showHex ammoInstrAddr "")
            --print $ "Recoil instr address: " ++ show (showHex recoilInstrAddr "")

            putStrLn ""

            -- Patch Infinite ammo
            --ammoInstrVal <- Mem.readInstruction pid ammoInstrAddr 3
            Mem.writeInstruction pid ammoInstrAddr (BS.pack $ replicate 3 0x90)
            -- Rollback patch
            --Mem.writeInstruction pid ammoInstrAddr $ fromMaybe (B8.pack "") ammoInstrVal

            -- No recoil
            --noRecoilInstrVal <- Mem.readInstruction pid recoilInstrAddr 6
            Mem.writeInstruction pid recoilInstrAddr (BS.pack $ replicate 6 0x90)
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


fstBotAddress :: Word64 -> Word64
fstBotAddress playerListAddress = playerListAddress + (fst . head $ readHex "8")

sndBotAddress :: Word64 -> Word64
sndBotAddress playerListAddress = playerListAddress + (fst . head $ readHex "10")

nextBotAddress :: Word64 -> Word64
nextBotAddress currentAddress = currentAddress + (fst . head $ readHex "10")