module Main where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import ApplicativeListExercise

main :: IO ()
main = do
  let trigger = undefined :: List (String, String, Int)
  quickBatch (applicative trigger)
