module Main where

import TextFractions
import Text.Trifecta

main :: IO ()
main = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
  print $ parseString parseIntegersEOF mempty "123"
  print $ parseString parseIntegersEOF mempty "123abc" --  expected to fail
  print $ parseString parseIntegersEOF' mempty "123"
  print $ parseString parseIntegersEOF' mempty "123abc" --  expected to fail
