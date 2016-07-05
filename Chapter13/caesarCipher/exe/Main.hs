module Main where

import System.IO
import CaesarCipher (caesarCipher)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Caesar Cipher"
  putStr "Enter word to be encrypted: "
  word <- getLine
  putStr "Enter offset: "
  number <- getLine
  putStrLn ("Encrypted word is: " ++ (caesarCipher (read number) word))
