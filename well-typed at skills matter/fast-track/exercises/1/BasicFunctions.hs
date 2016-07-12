module BasicFunctions where

import Prelude hiding (maximum, replicate, repeat, break, sum)
import Data.Maybe

--------------------------------------------------------------------------
-- Introduction:
--
-- This module contains additional small exercises on the concepts that
-- we have seen so far.

-- The function 'sum' should sum all the elements in a list of numbers.
-- Try to figure out the most general type yourself. Use GHCi to infer
-- it if you need to, but guess first.

--------------------------------------------------------------------------
-- More with lists:

sum ::Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs


sum' ::Num a => [a] -> a
sum' = go 0
       where
         go:: Num b => b -> [b] -> b
         go acc [] = acc
         go acc (x:xs) = go (acc + x) xs

sum'' :: Num a => [a] -> a
sum'' xs = foldr (+) 0 xs


-- Try to come up with two versions of 'sum'. One that recurses directly,
-- and one that makes use of an accumulating argument, akin to the two
-- versions of 'reverse'.

-- The following function is a reimplementation of the existing function
-- 'concat' that flattens a list of lists. However, the definition I'm
-- giving here contains mistakes and is overly complicated. Correct the
-- mistakes:
--
conc :: [[a]] -> [a]
conc []            =  []
conc (x : xs)  =  x ++ conc xs

conc' :: [[a]] -> [a]
conc' xs = foldr (++) [] xs

-- The function 'maximum' should compute the maximum of a non-empty
-- list of elements. You can use 'max' that computes the maximum of
-- two arguments.

maximum ::Ord a => [a] -> a
maximum [] = error "TODO: implement maximum"
--maximum [x]    = x
--maximum (x:xs) = max x (maximum xs)

maximum (x:xs) = go x xs
                 where
                   go:: Ord b => b -> [b] -> b
                   go acc [] = acc
                   go acc (x:xs) = go (max acc x) xs


maximum'' ::Ord a => [a] -> a
maximum'' [] = error "TODO: implement maximum"
maximum'' (x:xs) = foldr max x xs


maximum' ::Ord a => [a] -> Maybe a
maximum' [] = Nothing
maximum' (x:xs) = go x xs
                 where
                   go:: Ord b => b -> [b] -> Maybe b
                   go acc []     = Just acc
                   go acc (x:xs) = go (max acc x) xs

maximum''' ::Ord a => [a] -> Maybe a
maximum''' [] = Nothing
maximum''' (x:xs) = Just (foldr max x xs)


-- Try to come up with two versions of 'maximum', one that fails with
-- an exception on an empty list, and one that computes a 'Maybe'
-- result.

-- Implement a sorting algorithm. Any algorithm is fine. Insertion
-- sort is very straight-forward if you split the task into first
-- defining an insertion function for an already sorted list and
-- then the sorting function based on that. Merge sort and quick
-- sort are also relatively simple. Stick to the common principles!

sort :: Ord a => [a] -> [a]
sort xs = foldr insert [] xs 

insert :: Ord a => a -> [a] -> [a]
insert val [] = [val]
insert val (x:xs) = if val <= x then val:x:xs else x:insert val xs

-- Can't seem to implement insert using a foldr
insert' :: Ord a => a -> [a] -> [a]
insert' val xs = foldr (\x acc -> if val <= x then val:x:acc else x:acc) [] xs

sort' :: Ord a => [a] -> [a]
sort' xs = foldr insert'' [] xs
           where
             insert'' :: Ord a => a -> [a] -> [a]
             insert'' val [] = [val]
             insert'' val (x:xs) = if val <= x then val:x:xs else x:insert val xs             

--insert :: Ord a => a -> [a] -> [a]
--insert val [] = [val]
--insert val a = go val a []
--            where
--               go :: Ord a => a -> [a] -> [a] -> [a]
--               go val [] acc = acc ++ [val]
--               go val (x:xs) acc = if val < x then acc ++ [val] ++ (x:xs)
--                                                   else go val xs (acc ++ [x])


-- The function 'replicate' produces a list:
--
--   replicate n x
--
-- should be a list containing n copies of x.

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:(replicate (n-1) x)

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x) 


-- We can also create an infinite list of copies of one element:

repeat :: a -> [a]
repeat x = x : repeat x

-- Try to rewrite 'replicate' as a one-liner that uses
-- 'take' and 'repeat'.

-- By looking at the code and trying it on sample inputs, try to
-- figure out what the following function 'break' does:

break :: (a -> Bool) -> [a] -> ([a], [a])
break p []       = ([], [])
break p (x : xs)
  | p x          = ([], x : xs)
  | otherwise    = case break p xs of
                     (ys, zs) -> (x : ys, zs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p []       = ([], [])
break' p (x : xs)
 | p x          = ([], x : xs)
 | otherwise    = let (ys, zs) =  break' p xs in (x : ys, zs)

-- Rewrite 'break' slightly to use 'let'-'in' rather than 'case'-'of'.
-- For tuples (that have only a single constructor), deconstruction
-- via 'let' is rather common.
                   
-- Define a function group that groups subsequent equal elements
-- of a list into sublists.
--
-- Example:
--
--   group [1,1,1,2,3,3]
--
-- should yield
--
--   [[1,1,1],[2],[3,3]]
--
-- Note that in the result that 'group' produces, none of the
-- *inner* lists are empty, ever.
--
-- Hints:
--
-- You can still follow the standard design principle. You can
-- use 'break' in order to define 'group'.

--group :: Eq a => [a] -> [[a]]
--group xs = foldr (\x acc -> if x == head(head acc)
--                            then acc
--                            else (let (result, _) = break (==x) xs in acc ++ [result]))
--           (let (result, _) = break (/=(head xs)) xs in [result])
--           xs

--group :: Eq a => [a] -> [[a]]
--group x:xs = foldr (\x acc -> if x == head(head acc)
--                            then acc
--                            else (let (result, _) = break (==x) xs in acc ++ [result]))
--           [[x]]
--           xs

group :: Eq a => [a] -> [[a]]
group  []           =  []
group  (x:xs)       =  (x:ys) : group zs
                           where (ys,zs) = break (/=x) xs



-- Define a function 'encode' that defines the run-length encoding
-- of a list. The run-length encoding stores for every element how
-- often (in sequence) it occurs. For example,
--
--   encode "foo"
--
-- yields
--
--   [(1, 'f'), (2, 'o')]
--
-- We define a type synonym:

type Runlength a = [(Int, a)]

-- Now the function:

encode :: Eq a => [a] -> Runlength a
encode xs = map (\x -> (length x, head x)) (group xs)

-- Define a function 'decode' that decodes a run-length-encoded list
-- back to its original form:

--decode :: Runlength a -> [a]
decode xs = foldr (\x acc -> x ++ acc) [] (map (\(l, x) -> replicate l x) xs)

-- Try on a couple of examples that 'encode' followed by 'decode'
-- yields the original list.

--------------------------------------------------------------------------
-- Trees:

-- Let us introduce binary trees with labels in the leaves:

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Show) -- so that we can compare and print trees

-- What tree does the following value represent?

exTree :: Tree Int
exTree = Node (Leaf 1) (Node (Leaf 5) (Leaf 3))

-- Try in GHCi:
--
-- :t Leaf
-- :t Node
-- :i Tree

-- The following definition of 'size' is wrong. Fix it.
--
size :: Tree a -> Int
size (Leaf x)                 = 1
size (Node l r) = size l + size r

-- Define a function 'flatten' that produces a list of
-- all the elements in the tree, in order. For example,
--
--   flatten exTree
--
-- should yield
--
--   [1,5,3]

flatten :: Tree a -> [a]
flatten = error "TODO: define flatten"

-- Define a function that computes the height of a tree.

height :: Tree a -> Int
height = error "TODO: implement height"
-- Define a map-like function for trees that applies a given
-- function to every leaf while maintaining the shape of the
-- tree:

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = error "TODO: define mapTree"

--------------------------------------------------------------------------
-- Arithmetic expressions:

data Expr = Num Int
          | Add Expr Expr
  deriving (Eq, Show) -- so that we can compare and print expressions

exampleExpr :: Expr
exampleExpr = Add (Num 3) (Add (Num 5) (Num 1))

-- The above example expression is an abstract representation of
-- the expression
--
--   3 + (5 + 1)
--
-- Here is an evaluator for the expression type:

eval :: Expr -> Int
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2

-- And here is a function that produces a human-readable
-- string representation of an expression.

text :: Expr -> String
text (Num n)     = show n
text (Add e1 e2) = "(" ++ text e1 ++ " + " ++ text e2 ++ ")"

-- Now:
--
-- Add 'Mul' as a constructor representing multiplication
-- and extend 'eval' and 'text' to cover the new case.

--------------------------------------------------------------------------
-- Paths in trees:

-- Define a datatype |Path| that represents the sequence of steps
-- one can walk through a binary tree of type 'Tree' from the root
-- to a potential leaf. A path can either end, of we can go left
-- and continue, or we can go right and continue (so there should
-- be three constructors).

data Path = ToBeDefined -- placeholder, replace with a suitable definition
  deriving (Eq, Show)

-- Define a function 'follow' that takes a path and a tree and
-- tries to look up the element at the position described by the
-- path:

follow :: Path -> Tree a -> Maybe a
follow = error "TODO: implement follow"

-- Define a function 'search' that tries to find an element in
-- a tree and returns the path. Perhaps you can reuse the function
-- '(<|>)' discussed earlier:

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x  <|> y = Just x
Nothing <|> y = y

search :: Eq a => a -> Tree a -> Maybe Path
search = error "TODO: implement search"

