module Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)

stopVowelsStops :: [(Char, Char, Char)]
stopVowelsStops = combos stops vowels stops

-- Chapter 10 solution:
-- stopVowelsStops = [(a,b,c) | a <- stops, b <- vowels, c <- stops]
