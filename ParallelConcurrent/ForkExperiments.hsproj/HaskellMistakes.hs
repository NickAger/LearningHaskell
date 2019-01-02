module HaskellMistakes where
  
import Control.Monad.State
  
-- often put an equals by mistake after
-- fib n =
--   |  
--fib :: Int -> Int
--fib n
--    | n <= 1 = 1
--    | otherwise =
--        fib (n - 1) + fib (n - 2)
        
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib n = evalState (do
  forM [0..(n-1)] $ \_ -> do
    (a,b) <- get
    put (b,a+b)
  (a,b) <- get
  return a) (0, 1)