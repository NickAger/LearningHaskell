{-# OPTIONS_GHC -fno-defer-type-errors #-}

module Account4 where

import System.IO
import Control.Concurrent.STM

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    writeTVar acc (bal - amount)

bad :: Account -> IO ()
bad acc = do
    hPutStr stdout "Withdrawing..."
    withdraw acc 10
    --atomically (withdraw acc 10)

main = do
    acc <- atomically (newTVar 200)
    bad acc 
    hPutStr stdout "\nDone!\n"