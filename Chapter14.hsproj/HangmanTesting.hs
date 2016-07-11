{-# OPTIONS_GHC -Wall #-}

module HangmanTesting where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hangman
import Debug.Trace

puzzleGenerator :: Gen Puzzle
puzzleGenerator = do
    NonEmpty word <- arbitrary
    discoveredLetters <- generateDiscoveredLetters word
    let guessed = foldl fillInGuessCharacter [] discoveredLetters
    return (Puzzle word discoveredLetters guessed 0)
    where      
      generateDiscoveredLetters :: String -> Gen [Maybe Char]
      generateDiscoveredLetters word = do
        mapM discoveredLetter word
        where
          discoveredLetter :: Char -> Gen (Maybe Char)
          discoveredLetter c = do
             value <- generateBool
             return (if value == True then Just c else Nothing)
          generateBool :: Gen Bool
          generateBool = do
            value <- arbitrary
            return value
      fillInGuessCharacter :: [Char] -> Maybe Char -> [Char]      
      fillInGuessCharacter acc (Just c) = c : acc 
      fillInGuessCharacter acc Nothing = acc      
      
instance Arbitrary Puzzle where
  arbitrary = puzzleGenerator
  
-- TODO: why does it fail (ocassionally) with the test (initialDiscoveredLetters == discoveredLetters)
prop_fillInCharacter :: Puzzle -> Char -> Bool
prop_fillInCharacter puzzle@(Puzzle word _ guessed _) c
  | elem c guessed = alreadyGuessed puzzle updatedPuzzle
  | elem c word = checkFoundLetter puzzle updatedPuzzle
  | otherwise = checkNotFoundLetter puzzle updatedPuzzle
  where
    updatedPuzzle = fillInCharacter puzzle c
    alreadyGuessed (Puzzle _ initialDiscoveredLetters _ initialNumGuesses) (Puzzle _ discoveredLetters _ numGuesses) =
      (numGuesses == initialNumGuesses + 1) && 
      (
        (trace ("initialDiscoveredLetters: '" ++ (show initialDiscoveredLetters) ++ "'") initialDiscoveredLetters)
         == 
        (trace ("discoveredLetters: '" ++ (show discoveredLetters) ++ "'") discoveredLetters)
      ) 
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

   
  
     