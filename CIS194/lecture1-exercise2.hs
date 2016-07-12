
-- To move n discs (stacked in increasing size) from peg a to peg b 
-- using peg c as temporary storage,
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage. 

-- For this exercise, define a function hanoi with the following type:

-- type Peg = String
-- type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- Given the number of discs and names for the three pegs, hanoi should
-- return a list of moves to be performed to move the stack of discs from 
-- the first peg to the second.
-- Note that a type declaration, like type Peg = String above, 
-- makes a type synonym. 
-- In this case Peg is declared as a synonym for String, and the two names Peg and String 
-- can now be used interchangeably. 
-- Giving more descriptive names to types in this way can be used to give shorter 
-- names to complicated types, or (as here) simply to help with documentation.
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 peg1 peg2 _ = [(peg1, peg2)]
hanoi n peg1 peg2 peg3 = hanoi (n-1) peg1 peg3 peg2 ++ [(peg1, peg2)] ++ hanoi (n-1) peg3 peg2 peg1

-- Nick question
-- can I ask haskell to show the calls being made with the parameters being passed

