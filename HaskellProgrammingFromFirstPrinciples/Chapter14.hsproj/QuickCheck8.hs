{-# OPTIONS_GHC -Wall #-}

module QuickCheck8 where

import Test.QuickCheck
import Test.QuickCheck.Function

-- Write a property for the definition of ($).-- f $ a == f a
-- f . g == \x -> f (g x)

-- modified code in:
-- https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html#t:Function
-- told I should also look at CoArbitary -> https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Arbitrary.html
-- not quite sure what its doing...
prop_funcdollar :: Fun String Integer -> String -> Bool
prop_funcdollar (Fn f) x = (f $ x) == (f x)


-- not entirely sure these are testing what I intend to test
prop_funcdot :: Fun Integer [Integer] -> Fun String Integer -> String -> Bool
prop_funcdot (Fn f) (Fn g) x = (f . g) x == (\y -> f (g y)) x

runQc :: IO ()
runQc = do
  quickCheck prop_funcdollar
  quickCheck prop_funcdot
  