{-# OPTIONS_GHC -Wall #-}

module QuickCheck1 where

import Test.QuickCheck

-- QuickCheck exercise 1

half x = x / 2

-- this property should hold
halfIdentity = (*2) . half

prop_halfIdentity :: (Fractional a, Eq a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

runQc :: IO ()
runQc = quickCheck prop_halfIdentity

