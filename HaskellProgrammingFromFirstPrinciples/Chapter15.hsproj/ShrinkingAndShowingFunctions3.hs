{-# LANGUAGE ScopedTypeVariables #-}

module ShrinkingAndShowingFunctions3 where

-- from https://www.youtube.com/watch?v=CH8UQJiv9Q4
import Test.QuickCheck
import Test.QuickCheck.Function

prop_HaskellML :: (Fun String Bool) -> Property
prop_HaskellML  (Fn p)  =
  p "Haskell 98" ==> p "Standard ML"
