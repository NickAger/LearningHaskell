import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalisedLine = removeSpaces $ map toLower line1
  case (normalisedLine == reverse normalisedLine) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where
    removeSpaces word = filter (/= ' ') word
