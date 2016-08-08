{-# LANGUAGE OverloadedStrings #-}

module Main where

import TextFractions
import Data.Attoparsec.Text (parseOnly)
import Text.Trifecta

-- useful for playing around in GHCI
import Control.Applicative
import Text.Parsec (parse)

main :: IO ()
main = do
  -- parseOnly is Attoparsec
  print $ parseOnly parseFraction badFraction
  print $ parseOnly parseFraction shouldWork
  print $ parseOnly parseFraction shouldAlsoWork
  print $ parseOnly parseFraction alsoBad

  -- parseString is Trifecta
  print $ parseString parseFraction mempty badFraction
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
