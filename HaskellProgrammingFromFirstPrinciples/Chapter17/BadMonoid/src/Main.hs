module Main where

import BadMonoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

test :: Bull
test = Twoo

main :: IO ()
main = quickBatch (monoid Twoo)
