module WhatHappens where
  
import Control.Concurrent
import System.IO.Unsafe



myDataIO :: IO (MVar Int)
myDataIO = newEmptyMVar

deadlock :: IO ()
deadlock = do
  mv <- myDataIO
  putMVar mv 0
  mv' <- myDataIO
  zero <- takeMVar mv'
  print zero


normalMVar = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero

myDataUnsafeIO :: MVar Int
myDataUnsafeIO = unsafePerformIO newEmptyMVar

mVarUnsafeIO ::IO ()
mVarUnsafeIO = do
  putMVar myDataUnsafeIO 0
  zero <- takeMVar myDataUnsafeIO
  print zero