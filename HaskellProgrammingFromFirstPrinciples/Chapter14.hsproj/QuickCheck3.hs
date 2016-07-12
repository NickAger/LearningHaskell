{-# OPTIONS_GHC -Wall #-}

module QuickCheck3 where

import Test.QuickCheck

-- Now we’ll test the associative and commutative properties of addition:

plusAssociative x y z =  x + (y + z) == (x + y) + z
  
plusCommutative x y =
  x + y == y + x
  
prop_associative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_associative = plusAssociative

prop_commutative :: (Eq a, Num a) => a -> a -> Bool
prop_commutative = plusCommutative


runQc :: IO ()
runQc = do
  quickCheck prop_associative
  quickCheck prop_commutative