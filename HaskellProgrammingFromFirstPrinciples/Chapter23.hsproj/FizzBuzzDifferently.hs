module FizzBuzzDifferently where
  
-- Exercise
-- Rather than changing the underlyng data structure, fix our reversing fizzbuzz by changing the code in the following way:
-- fizzbuzzFromTo :: Integer -> Integer -> [String]

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
  

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start finish
   | start >= finish = fizzbuzzList $ enumFromTo start finish
   | finish > start = fizzbuzzList $ enumFromThenTo finish (finish - 1) start
  
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
  

-- alternative

  
                                                                                                                                                                                                                                                                                                                                                          