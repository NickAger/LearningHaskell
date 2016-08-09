module Main where

import PhoneNumberParsing
import Text.Trifecta

examples = ["123-456-7890", "1234567890", "(123) 456-7890", "1-123-456-7890"]


main :: IO ()
main = mapM_ (print . parseString parsePhone mempty) examples
