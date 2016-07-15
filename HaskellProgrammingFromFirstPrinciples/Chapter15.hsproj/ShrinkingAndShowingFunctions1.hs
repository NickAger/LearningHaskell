{-# LANGUAGE ScopedTypeVariables #-}

module ShrinkingAndShowingFunctions1 where

-- from https://www.youtube.com/watch?v=CH8UQJiv9Q4
import Test.QuickCheck
import Test.QuickCheck.Poly
import Text.Show.Functions

-- A lot of this 
prop_MapFilter f p (xs::[A]) =
  whenFail (print (table f xs)) $
    whenFail (print (table p (xs ++ map f xs))) $
      map f (filter p xs) == filter p (map f xs)


table h xs = [ (x, h x) | x <- xs]