module Main where

import LogFileParser
import Text.Trifecta

main :: IO ()
main = do
  print $ parseString (parseUpToLoggedDay *> parseLoggedDay) mempty logSample
--  print $ parseString parseLogFile mempty logSample
