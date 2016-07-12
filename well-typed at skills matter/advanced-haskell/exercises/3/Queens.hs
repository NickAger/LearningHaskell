import Control.Monad
import System.Environment

type PartialSolution = [Int] -- per column, list the row the queen is in
type Solution = PartialSolution

type BoardSize = Int

-- Generate all solutions for a given board size.
queens :: BoardSize -> [Solution]
queens n = iterate (concatMap (addQueen n)) [[]] !! n

-- Given the size of the problem and a partial solution for the
-- first few columns, find all possible assignments for the next
-- column and extend the partial solution.
addQueen :: BoardSize -> PartialSolution -> [PartialSolution]
addQueen n s = [ x : s | x <- [1..n], safe x s 1 ]

-- Given a row number, a partial solution and an offset, check
-- that a queen placed at that row threatens no queen in the
-- partial solution.
safe :: Int -> PartialSolution -> Int -> Bool
safe x []    n = True
safe x (c:y) n = x /= c && x /= c + n && x /= c - n && safe x y (n + 1)

main = do
         [n] <- getArgs
         print $ length $ queens (read n)
