module Main where

import Test.QuickCheck
import ArbitraryIdentity
import ArbitraryProducts
import ArbitrarySum
import CoArbitrary

data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample trivialGen
