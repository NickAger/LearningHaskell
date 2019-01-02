module Account5 where

import System.IO
import Control.Concurrent.STM

launchMissiles :: IO ()
launchMissiles = hPutStr stdout "Zzzing!"

main = do 
    xv <- atomically (newTVar 2)
    yv <- atomically (newTVar 1)
    atomically (do x <- readTVar xv
                   y <- readTVar yv
                   if x > y then launchMissiles
                            else return () )