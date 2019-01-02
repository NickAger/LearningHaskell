-- MVar version2

-- to try to avoid the problem of the transfer occuring
-- atomically, we've created a worse mess:
-- * transfer no longer uses `withdraw` and `deposit` (broken composition)
-- * transfer with accounts in a different order will cause a deadlock
--
-- "locks don't compose."

module Account1 where
  
import Control.Concurrent
  
type Account = MVar Int

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount 
    = do 
        bal1 <- takeMVar from
        bal2 <- takeMVar to
        
        putMVar from (bal1 - amount)
        putMVar to (bal2 + amount)

showAccount :: Account -> IO Int
showAccount acc = readMVar acc

main = do
    from <- newMVar 200
    to   <- newMVar 100
    transfer from to 50
    v1 <- showAccount from
    v2 <- showAccount to
    putStrLn $ (show v1) ++ ", " ++ (show v2)