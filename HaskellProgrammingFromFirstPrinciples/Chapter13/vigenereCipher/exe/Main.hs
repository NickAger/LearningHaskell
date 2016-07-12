module Main where

import System.IO
import VigenereCipher (vigenèreCipher)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Vigenère Cipher"
  putStr "Enter phrase to be encrypted: "
  phrase <- getLine
  putStr "Enter seed word or phrase: "
  seed <- getLine
  putStrLn ("Encryption is: " ++ (vigenèreCipher seed phrase))
