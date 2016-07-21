module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Identity
import Constant
import Optional
import List
import Three
import Three'
import S
import Tree

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Constant String (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three String Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three' String (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: S Maybe (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
