{-# OPTIONS_GHC -Wall #-}

module FailureExercise where

import Test.QuickCheck

-- Failure
-- Find out why this property fails.
-- for a function
square x = x * x
-- why does this property no hold? Examine the type of sqrt.
squareIdentity = square . sqrt

prop_squareIdentity :: (Floating a, Eq a) => a -> Bool
prop_squareIdentity x = squareIdentity x == x
