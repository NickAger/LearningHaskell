{-# OPTIONS_GHC -Wall #-}

module Idempotence where

import Test.QuickCheck
import Data.Char (toUpper)
import Data.List (sort)


twice f = f . f
fourTimes = twice . twice

--


capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs


f1 x = (capitalizeWord x == twice capitalizeWord x) &&  (capitalizeWord x == fourTimes capitalizeWord x)

prop_capitalizationIdemptence ::  String -> Bool
prop_capitalizationIdemptence x = f1 x 

--

f2 :: (Ord a) => [a] -> Bool
f2 x = (sort x == twice sort x) && (sort x == fourTimes sort x)


prop_sortIdemptence :: [Int] -> Bool
prop_sortIdemptence x = f2 x 