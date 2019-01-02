import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad.Par.Scheds.Trace
import Control.Concurrent

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib1 = force $ (!!) fibs

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

solutions = map fib [100..110]

parSolutions = solutions `using` parList rseq


test n m = runPar $ do
  i <- new                          
  j <- new                          
  fork (put i (fib n))              
  fork (put j (fib m))              
  a <- get i                        
  b <- get j                        
  return (a+b)  
  
main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  rm <- takeMVar m
  print rm