module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 6

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let
          l = length(w :: String)
      in
          l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl -  1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed numGuesses) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++  " Guessed so far: " ++ guessed ++ show numGuesses

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) char = elem char word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just char) = char
renderPuzzleChar Nothing = '_'


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s numGuesses) c = Puzzle word newFilledInSoFar ( c : s ) (numGuesses + 1)
  where
    zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO()
gameOver (Puzzle wordToGuess _ _ numGuesses) =
    if numGuesses > 20 then
      do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
    else
      return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ fillInSoFar _ _) =
  if all isJust fillInSoFar then
    do
      putStrLn "You win!"
      exitSuccess
  else
    return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
