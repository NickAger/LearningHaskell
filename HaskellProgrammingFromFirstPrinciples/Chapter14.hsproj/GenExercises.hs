{-# OPTIONS_GHC -Wall #-}

module GenExercises where

import Test.QuickCheck

data Fool = 
    Fulse
  | Frue
  deriving (Eq, Show)

-- Ex1:
-- Equal probabilities for each.

equalProbability :: Gen Fool
equalProbability = do
  oneof [return Fulse, return Frue]
  
-- Ex2:
-- 2/3s chance of Fulse, 1/3 chance of Frue.

unEqualProbability :: Gen Fool
unEqualProbability = do
    oneof [return Fulse, return Fulse, return Frue]

unEqualProbability2 :: Gen Fool
unEqualProbability2 = do
    frequency [(2, return Fulse), (1, return Frue)]
