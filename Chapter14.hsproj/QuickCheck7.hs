{-# OPTIONS_GHC -Wall #-}

module QuickCheck7 where

import Test.QuickCheck

-- Is (^) associative? Is it commutative? Use QuickCheck to see if the computer can contradict such an assertion.

powerAssociative x y z =  x ^ (y ^ z) == (x ^ y) ^ z
  
powerCommutative x y =
  x ^ y == y ^ x
  
prop_associative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_associative = powerAssociative

prop_commutative :: (Eq a, Integral a) => a -> a -> Bool
prop_commutative = powerCommutative

runQc :: IO ()
runQc = do
  quickCheck prop_associative
  quickCheck prop_commutative