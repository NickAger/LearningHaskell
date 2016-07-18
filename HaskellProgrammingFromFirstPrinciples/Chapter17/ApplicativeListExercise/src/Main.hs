module Main where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import ApplicativeListExercise

main :: IO ()
main = do
  -- let trigger = undefined ::  [(ZipList' (List String), ZipList' (List Int), ZipList' (List String))]
  let trigger = undefined :: ZipList' (List String, List Int, List String)
  quickBatch (applicative trigger)
