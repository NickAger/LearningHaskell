{-# OPTIONS_GHC -Wall #-}

module QuickCheck10 where

import Test.QuickCheck

-- Hm. Is that so?
-- f n xs = length (take n xs) == n


prop_lengthTake :: Int -> [Int] -> Bool
prop_lengthTake n xs = length (take n xs) == n