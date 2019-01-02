module Account6 where

import System.IO
import Control.Concurrent.STM
import Control.Concurrent
  
type Account = TVar Int

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
    bal <- readTVar acc
    if amount > 0 && amount > bal
    then retry
    else writeTVar acc (bal - amount)

delayDeposit acc amount = do
    hPutStr stdout "Getting ready to deposit money...hunting through pockets...\n"
    threadDelay 3000000
    hPutStr stdout "OK! Depositing now!\n"
    atomically ( do bal <- readTVar acc
                    writeTVar acc (bal + amount) )

main = do
    acc <- atomically (newTVar 100)
    forkIO (delayDeposit acc 1)
    hPutStr stdout "Trying to withdraw money...\n"
    atomically (limitedWithdraw acc 101)
    hPutStr stdout "Successful withdrawal!\n"