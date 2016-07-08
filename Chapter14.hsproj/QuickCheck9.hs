{-# OPTIONS_GHC -Wall #-}

module QuickCheck9 where

import Test.QuickCheck

-- See if these two functions are equal:
--   foldr (:) == (++)
--   foldr (++) [] == concat


-- note I had to switch the order of the parameters so they are not equivalent!
prop_foldrConcat :: [Int] -> [Int] -> Bool
prop_foldrConcat xs ys = foldr (:) ys xs == (++) xs ys

prop_foldrConcat2 :: [[Int]] -> Bool
prop_foldrConcat2 xs = foldr (++) [] xs == (concat) xs