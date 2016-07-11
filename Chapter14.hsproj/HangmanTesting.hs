{-# OPTIONS_GHC -Wall #-}

module HangmanTesting where

import Test.QuickCheck
--import Test.QuickCheck.Monadic
import Hangman
import Control.Monad

puzzleGenerator :: Gen Puzzle
puzzleGenerator = do
    NonEmpty word <- arbitrary
    guessed <- generateGuessLetters word
    let discoveredLetters = fillInDiscoveredLetters word guessed
    return (Puzzle word discoveredLetters guessed 0)
    where
      generateGuessLetters :: String -> Gen [Char]
      generateGuessLetters word = filterM includeCharacter word        
        where 
          includeCharacter :: Char -> Gen Bool
          includeCharacter _ = arbitrary
      
      fillInDiscoveredLetters :: String -> [Char] -> [Maybe Char]
      fillInDiscoveredLetters word guessed = 
        map fillInGuessCharacter word
        where
          fillInGuessCharacter c 
            | elem c guessed = Just c
            | otherwise = Nothing   
      
instance Arbitrary Puzzle where
  arbitrary = puzzleGenerator
  
prop_fillInCharacter :: Puzzle -> Char -> Bool
prop_fillInCharacter puzzle@(Puzzle word _ guessed _) c
  | elem c guessed = alreadyGuessed puzzle updatedPuzzle
  | elem c word = checkFoundLetter puzzle updatedPuzzle
  | otherwise = checkNotFoundLetter puzzle updatedPuzzle
  where
    updatedPuzzle = fillInCharacter puzzle c
    alreadyGuessed (Puzzle _ initialDiscoveredLetters _ initialNumGuesses) (Puzzle _ discoveredLetters _ numGuesses) =
      (numGuesses == initialNumGuesses + 1) && initialDiscoveredLetters == discoveredLetters
    checkFoundLetter (Puzzle _ _ _ initialNumGuesses) (Puzzle _ discoveredLetters guesses numGuesses) = 
      elem (Just c) discoveredLetters && (numGuesses == initialNumGuesses + 1) && elem c guesses 
    checkNotFoundLetter (Puzzle _ _ _ initialNumGuesses) (Puzzle _ discoveredLetters guesses numGuesses) = 
      not (elem (Just c) discoveredLetters) && (numGuesses == initialNumGuesses + 1) && elem c guesses 

prop_handleGuess :: Puzzle -> Char -> IO Bool
prop_handleGuess puzzle@(Puzzle word _ guessed _) c
  | elem c guessed = alreadyGuessed puzzle
  | elem c word = checkFoundLetter puzzle
  | otherwise = checkNotFoundLetter puzzle
  where
    alreadyGuessed initialPuzzle = do
      updatedPuzzle <- handleGuess puzzle c
      return (initialPuzzle == updatedPuzzle)
    checkFoundLetter (Puzzle _ _ _ initialNumGuesses) = do
      (Puzzle _ discoveredLetters guesses numGuesses) <- handleGuess puzzle c
      return (elem (Just c) discoveredLetters && (numGuesses == initialNumGuesses + 1) && elem c guesses) 
    checkNotFoundLetter (Puzzle _ _ _ initialNumGuesses) = do
      (Puzzle _ discoveredLetters guesses numGuesses) <- handleGuess puzzle c
      return (not (elem (Just c) discoveredLetters) && (numGuesses == initialNumGuesses + 1) && elem c guesses) 
    
--prop_handleGuess2 puzzle c = monadicIO $ do
--  (_, _, result) <- run (prop_handleGuess puzzle c)
--  return True

   
  
     