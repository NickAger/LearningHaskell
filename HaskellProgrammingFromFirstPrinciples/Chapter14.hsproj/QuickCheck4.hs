{-# OPTIONS_GHC -Wall #-}

module QuickCheck4 where

import Test.QuickCheck

-- Now we’ll test the associative and commutative properties of multiplication:

multiplicationAssociative x y z =  x * (y * z) == (x * y) * z
  
multiplicationCommutative x y =
  x * y == y * x
  
prop_associative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_associative = multiplicationAssociative

prop_commutative :: (Eq a, Num a) => a -> a -> Bool
prop_commutative = multiplicationCommutative


runQc :: IO ()
runQc = do
  quickCheck prop_associative
  quickCheck prop_commutative