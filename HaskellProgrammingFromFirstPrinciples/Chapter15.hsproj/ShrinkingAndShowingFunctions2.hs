{-# LANGUAGE ScopedTypeVariables #-}

module ShrinkingAndShowingFunctions2 where

-- from https://www.youtube.com/watch?v=CH8UQJiv9Q4
import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.Function

prop_MapFilter (Fun f) (Fun p) (xs::[A]) =
  map f (filter p xs) == filter p (map f xs)
