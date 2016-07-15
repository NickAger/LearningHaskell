{-# LANGUAGE ScopedTypeVariables #-}

module ShrinkingAndShowingFunctions3 where

-- from https://www.youtube.com/watch?v=CH8UQJiv9Q4
import Test.QuickCheck
import Test.QuickCheck.Function
import Text.Show.Functions

prop_HaskellML (Fun _ p)  =
  p "Haskell 98" ==> p "Standard ML"
