-- from:
-- http://projects.haskell.org/diagrams/gallery/KnightTour.html
-- lots of good examples here: http://rosettacode.org/wiki/Knight%27s_tour#Haskell

{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List                      (minimumBy, tails, (\\))
import           Data.Ord                       (comparing)
import           Diagrams.Prelude

type Square = (Int, Int)
 
board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]

knightMoves :: Square -> [Square]
knightMoves (x,y) = filter (`elem` board) jumps
  where jumps = [ (x+i,y+j) | i <- jv, j <- jv, abs i /= abs j ]
        jv    = [1,-1,2,-2]

knightTour :: Square -> [Square]
knightTour sq = knightTour' [sq]
  where
    knightTour' moves@(lastMove:_)
        | null candMoves = reverse moves
        | otherwise = knightTour' $ newSquare : moves
      where newSquare   = minimumBy (comparing (length . findMoves)) candMoves
            candMoves   = findMoves lastMove
            findMoves s = knightMoves s \\ moves
            

-- from http://learnyouahaskell.com/a-fistful-of-monads
-- A knight's quest
knightMoves2 :: Square -> [Square]  
knightMoves2 (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
