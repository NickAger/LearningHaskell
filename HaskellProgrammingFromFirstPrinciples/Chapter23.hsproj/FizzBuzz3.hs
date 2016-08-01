module FizzBuzz3 where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL
import Data.Foldable

fizzBuzz :: Integer -> String
fizzBuzz n 
           | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n
           
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty
  
addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)
  
main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList [1..100]
  
 