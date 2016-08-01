module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n 
           | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n
           
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []
  
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
main :: IO ()
main =
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  
 
-- execState :: State s a -> s -> s
-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--  mapM :: Monad m => (a -> m b) -> t a -> m (t b)

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

-- http://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell
-- The core idea is that mapM maps an "action" (ie function of type a -> m b) over a list and gives you all the results as a m [b]. mapM_ does the same thing, but never collects the results, returning a m ().