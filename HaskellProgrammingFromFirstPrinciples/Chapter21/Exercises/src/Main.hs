module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TI = []

main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
