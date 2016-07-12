{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter id $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ys = map (length . flip elemIndices ys) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith min (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess = Move guess exactMatchScore (matches code guess - exactMatchScore)
    where exactMatchScore  = exactMatches code guess


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code exactScore regularScore) guess =  actualExactScore == exactScore && actualRegularScore == regularScore
    where (Move _ actualExactScore actualRegularScore) = getMove guess code

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move) 

-- Exercise 6 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 1 = map (:[]) colors
allCodes n = increaseCodeLength $ allCodes $ n - 1
    where
        increaseCodeLength :: [Code] -> [Code]
        increaseCodeLength codes = [ x:y | x <- colors, y <- codes]
        -- increaseCodeLength codes = concatMap (\x -> (map (:x) colors)) codes 

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = reverse $ makeMoves (allCodes (length code)) []
    where 
        makeMoves :: [Code] -> [Move] -> [Move]
        makeMoves [] _ = error "Cannot have zero possible moves"
        makeMoves (guess:possibleCodes) movesSoFar = if isWinningMove move then moves else makeMoves filteredCodes moves
            where 
                move = getMove code guess
                moves =  move : movesSoFar
                filteredCodes = filterCodes move possibleCodes
                isWinningMove (Move _ exactMatchScore _) = exactMatchScore == length code  

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

data FailableDouble = Failure
                    | OK Double
  deriving Show