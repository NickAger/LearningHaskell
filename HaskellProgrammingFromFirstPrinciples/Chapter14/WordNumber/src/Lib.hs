module Lib where

import Data.List (intercalate)

-- from Chapter 8

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

-- works - just a little ugly
--digits :: Int -> [Int]
--digits n = map (\c -> (read [c]) :: Int) (show n)

digits :: Int -> [Int]
digits n = go n []
  where go n acc
          | n < 10 = mod n 10 : acc
          | otherwise = go (div n 10) (mod n 10 : acc)

wordNumber :: Int -> String
wordNumber n = intercalate "-" digitsArray
  where digitsArray = map digitToWord (digits n)
