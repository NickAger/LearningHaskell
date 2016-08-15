module Main where

import LogFileParser
import Text.Trifecta

main :: IO ()
main = do
  print $ parseString parseLogFile mempty logSample
  print $ sumActivitiesLog logSample
