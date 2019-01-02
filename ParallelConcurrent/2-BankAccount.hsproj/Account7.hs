module Account7 where
  
import System.IO
import Control.Concurrent.STM
import Control.Concurrent

type Account = TVar Int

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
    bal <- readTVar acc
    check (amount <= 0 || amount <= bal)
    writeTVar acc (bal - amount) 

showAcc name acc = do
    bal <- atomically (readTVar acc)
    hPutStr stdout (name ++ ": $")
    hPutStr stdout (show bal ++ "\n")

limitedWithdraw2 :: Account -> Account -> Int -> STM ()
-- (limitedWithdraw2 acc1 acc2 amt) withdraws amt from acc1,
-- if acc1 has enough money, otherwise from acc2.
-- If neither has enough, it retries.
limitedWithdraw2 acc1 acc2 amt
  = orElse (limitedWithdraw acc1 amt) (limitedWithdraw acc2 amt)
  
delayDeposit name acc amount = do
    threadDelay 3000000
    hPutStr stdout ("Depositing $" ++ show amount ++ " into " ++ name ++ "\n")
    atomically ( do bal <- readTVar acc
                    writeTVar acc (bal + amount) )

main = do
    acc1 <- atomically (newTVar 100)
    acc2 <- atomically (newTVar 100)
    showAcc "Left pocket" acc1
    showAcc "Right pocket" acc2
    forkIO (delayDeposit "Right pocket" acc2 1)
    hPutStr stdout "Withdrawing $101 from either pocket...\n"
    atomically (limitedWithdraw2 acc1 acc2 101)
    hPutStr stdout "Successful withdrawal!\n"
    showAcc "Left pocket" acc1
    showAcc "Right pocket" acc2