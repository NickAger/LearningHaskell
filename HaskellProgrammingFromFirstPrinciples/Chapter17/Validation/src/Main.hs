module Main where

import Data.Either.Validation
import Validation
-- for some unknown reason I couldn't import Test.HUnit



main :: IO ()
main = do
  putStr "success == Success 2 - "
  print $ success == Success 2
  putStr "failure == Failure [StackOverflow] - "
  print $ failure == Failure [StackOverflow]
  putStr "failure' == Failure [StackOverflow] - "
  print $ failure' == Failure [StackOverflow]
  putStr "failures == Failure [MooglesChewedWires, StackOverflow] - "
  print $ failures == Failure [MooglesChewedWires, StackOverflow]
