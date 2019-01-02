module BlockedExceptionMVar where
 
import Control.Concurrent

-- should cause a `BlockedIndefinitelyOnMVar` exception - 
-- "thread bocked indefinitely in an MVar operation"
main = do
  m <- newEmptyMVar
  takeMVar m
  
