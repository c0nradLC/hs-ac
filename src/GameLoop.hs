{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameLoop () where
import Data.Word (Word64)
import Memory (readInt32, readAddress)
import Control.Monad (forever, forM_)
import Control.Concurrent (threadDelay)
import Numeric (readHex, showHex)
    -- Frame iteration using a polling loop
gameLoop :: Int -> Word64 -> IO ()
gameLoop pid gameModuleBaseAddr = do
    forever $ do
        mPlayerListAddr <- readAddress pid (playerListPointer gameModuleBaseAddr)

        case mPlayerListAddr of
            Just playerListAddress -> do
                let playerListAddress = playerListAddress + (fst . head $ readHex "8")
                print $ "PLayer list address: " ++ show (showHex playerListAddress "")

                -- Read player list count
                mPlayerCount <- readInt32 pid (playerListPointer gameModuleBaseAddr)

                return ()

            Nothing -> return ()

        -- Sleep to avoid 100% CPU
        threadDelay 166  -- ~60 FPS (16.666ms)

playerEntityPointer :: Word64 -> Word64
playerEntityPointer gameModuleBaseAddr = gameModuleBaseAddr + (fst . head $ readHex "19d518")

playerListPointer :: Word64 -> Word64
playerListPointer gameModuleBaseAddr = gameModuleBaseAddr + (fst . head $ readHex "19d520")


