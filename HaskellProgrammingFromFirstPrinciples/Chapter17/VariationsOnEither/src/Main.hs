module Main where

import VariationsOnEither
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  -- understanding the types passed to applicative function below
  -- see http://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr/36012069#36012069
  -- applicative is defined like this (simplified):
  -- applicative :: m (a,b,c) -> TestBatch
  --                (1)  (2)
  -- (1) monad is this case it is "Sum String" or "Validation String"
  --  we give a specific instance to the first type so that it is a type
  --  with single type parameter
  -- (2) tuple of values that will be used to generate the functions.
  quickBatch $ applicative (undefined :: Sum String (Int, Double, Char))
  quickBatch $ applicative (undefined :: Validation String (Int, Double, Char))
