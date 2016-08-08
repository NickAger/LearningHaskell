module Main where

import PositiveInteger
import Text.Trifecta

main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc" -- expected to fail
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"  -- expected to fail
  print $ parseString base10Integer' mempty "123abc"
  print $ parseString base10Integer' mempty "-123abc"
