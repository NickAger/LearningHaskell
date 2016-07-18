module Main where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Data.Monoid
import Apl1

main :: IO ()
main = quickBatch $ monoid (ZipList [1 :: Sum Int])
