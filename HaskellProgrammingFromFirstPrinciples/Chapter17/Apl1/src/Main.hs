module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Data.Monoid
import Apl1
import ListApplicativeExercise


main :: IO ()
main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  let trigger = undefined :: List (String, String, Int)
  quickBatch (applicative trigger)
