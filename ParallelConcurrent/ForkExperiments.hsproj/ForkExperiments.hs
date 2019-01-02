module ForkExperiments where 
  
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 10 (putChar 'A'))
  replicateM_ 10 (putChar 'B')
  