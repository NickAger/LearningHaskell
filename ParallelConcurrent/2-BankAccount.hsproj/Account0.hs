-- MVar version
-- breaks invariant transfer does not occur atomically

module Account0 where
  
import Control.Concurrent
  
type Account = MVar Int

withdraw :: Account -> Int -> IO ()
withdraw acc amount = do
    bal <- takeMVar acc
    putMVar acc (bal - amount)

deposit :: Account -> Int -> IO ()
deposit acc amount = withdraw acc (- amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount 
    = do 
        withdraw from amount
        deposit to amount

showAccount :: Account -> IO Int
showAccount acc = readMVar acc

main = do
    from <- newMVar 200
    to   <- newMVar 100
    transfer from to 50
    v1 <- showAccount from
    v2 <- showAccount to
    putStrLn $ (show v1) ++ ", " ++ (show v2)