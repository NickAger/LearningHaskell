module ForkMVars where
  
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r  
  
  

  