module Main where

import AltParsing
import Text.Trifecta

a = "blah"
b = "123"
c = "123blah789"

-- many is "zero or more" and some is "one or more"
main :: IO ()
main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c

  print $ parseString (some (token parseNos)) mempty eitherOr
