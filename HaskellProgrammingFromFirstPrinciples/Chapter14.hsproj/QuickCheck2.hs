{-# OPTIONS_GHC -Wall #-}

module QuickCheck2 where

import Test.QuickCheck
import Data.List (sort)

-- QuickCheck exercise 2

-- for any list you apply sort to-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where 
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t) 
    go y (Just x, _) = (Just y, x >= y)
    
-- this property should hold
prop_sort :: (Ord a) => [a] -> Bool
prop_sort = listOrdered . sort

runQc :: IO ()
runQc = quickCheck prop_sort