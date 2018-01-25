module CaesarCipher where

import Data.Char

caesarCipher ::  Int -> String -> String
caesarCipher n s = map (\c -> chr (ord 'a' + ((ord . toLower $ c) - ord 'a' + n) `mod` 26)) s


uncaesarCipher ::  Int -> String -> String
uncaesarCipher n s = map (\c -> chr (ord 'a' + ((ord . toLower $ c) - ord 'a' - n) `mod` 26)) s 
