module Main where

import SemVer
import Text.Trifecta

examples = ["1.0.0-alpha+001", "1.0.0+20130313144700", "1.0.0-beta+exp.sha.5114f85", "1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-0.3.7", "1.0.0-x.7.z.92"]

main :: IO ()
main = do
  print $ parseString parseSemVerWithNoReleaseMetadata mempty "2.1.1"
  print $ parseString parseSemVerWithNoReleaseMetadata' mempty "2.1.2"
  mapM_ (print . parseString parseSemVer mempty) examples
