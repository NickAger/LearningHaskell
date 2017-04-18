import Control.Monad

{-
The line: `q <- foldr1 mplus (map return [1..8])`
is equivalent to: `q <- [1..8]` for the *List* monad.
The first implementation is general to any MonadPlus.

Similarly, `if safe q qs then return (q:qs) else mzero`
is equivalent to:  `guard (safe q qs); return (q:qs)`
Note: mzero can be replaced with []
-}

queens :: Integer -> [[Integer]]
queens 0 = return []
queens n =
  do
    qs <- queens (n-1)
--    q <- foldr1 mplus (map return [1..8])

    q <- [1..8]
--    if safe q qs then return (q:qs) else mzero
    guard (safe q qs)
    return (q:qs)
    
safe :: Integer -> [Integer] -> Bool
safe _ [] = True
safe y qs = 
    let 
      noHorizontalMatch = y `notElem` qs
      
      tupleMatch t = fst t == snd t
      
      nwYPositions = enumFromThen (y - 1) (y - 2)
      nwYPositionsCompared = zip nwYPositions qs
      noNWMatch = not $ any tupleMatch nwYPositionsCompared
      
      swYPositions = enumFromThen (y + 1) (y + 2)
      swYPositionsCompared = zip swYPositions qs
      noSWMatch = not $ any tupleMatch swYPositionsCompared      
    in
      noHorizontalMatch && noNWMatch && noSWMatch
