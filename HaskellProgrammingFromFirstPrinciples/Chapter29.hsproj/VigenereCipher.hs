module VigenereCipher where

import Data.Char
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, stdout, stdin)
import System.Exit (exitFailure)

{-
A Caesar cipher is a simple substitution cipher, in which each letter is replaced by the letter that is a  fixed number of places down the alphabet from it. You will find variations on this all over the place — you can shift leftward or rightward, for any number of spaces. A rightward shift of 3 means that
’A’ will become ’D’ and ’B’ will become ’E,’ for example. If you did a leftward shift of 5, then ’a’ would become ’v’ and so forth.

A Vigenère cipher is another substitution cipher, based on a Caesar cipher, but it uses a series of Caesar ciphers for polyalphabetic substitution. The substitution for each letter in the plaintext is determined by a fixed keyword.
So, for example, if you want to encode the message “meet at dawn,” the  first step is to pick a keyword that will determine which Caesar cipher to use. We’ll use the keyword “ALLY” here. You repeat the keyword for as many characters as there are in your original message:
MEET AT DAWN
ALLY AL LYAL
Now the number of rightward shifts to make to encode each character is set by the character of the keyword that lines up with it. The ’A’ means a shi  of 0, so the initial M will remain M. But the ’L’ for our second character sets a rightward shi  of 11, so ’E’ becomes ’P’. And so on, so “meet at dawn” encoded with the keyword “ALLY” becomes “MPPR AE OYWY.”

-}

{-
Exercise:

make an executable that takes a key and a mode argument. If the mode is -d the executable decrypts the input from standard in and writes the decrypted text to standard out. If the mode is -e the executable blocks on input from standard input (stdin) and writes the encrypted output to stdout.
-}
    
newtype Seed = Seed String
newtype Message = Message String
newtype EncodedMessage = EncodedMessage String deriving Show

vigenèreCipher :: Seed -> Message -> EncodedMessage
vigenèreCipher (Seed seed) (Message message) = 
  let
    repeatingSeed = cycle $ map toUpper seed
    (encodedMessage, _) = foldl processCharacter ("", repeatingSeed) message 
  in
    (EncodedMessage encodedMessage)
  where
    processCharacter :: (String, String) -> Char -> (String, String)
    processCharacter (encodeMsg, repeatingSeed@(s:sx)) c  =
      let 
        cipherInt = (ord s) - (ord 'A')
        (encoded, remainingSeed) = if (c == ' ') 
          then (encodeMsg ++ " ", repeatingSeed) 
          else (encodeMsg ++ [applyCipherToChar c cipherInt], sx)
      in
        (encoded, remainingSeed)
        
    applyCipherToChar :: Char -> Int -> Char
    applyCipherToChar c s = chr (ord 'A' + ((ord . toUpper $ c) - ord 'A' + s) `mod` 26)
    

readFromStandardInput :: String -> IO ()
readFromStandardInput key = undefined

blocksOnInput :: String -> IO ()
blocksOnInput key = undefined

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [key, mode] ->
      case mode of
        "-d"    -> readFromStandardInput key
        "-e"    -> blocksOnInput key
        _       -> argError
    _ -> argError

  where argError = do
          putStrLn "usage: VigenereCipher key option\nOption either:\n    -d decrypts the input from standard in\n    -e blocks on input from standard input"
          exitFailure