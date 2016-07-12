module Poly where

import Prelude hiding ((.), foldr, foldl, const)
import Data.Functor
import qualified Data.Map as M
import Data.Map (Map)

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the part on "Polymorphism and higher-order
-- functions".
--
-- Not all the code is included this time, but there are a few examples
-- and exercises in here.

--------------------------------------------------------------------------
-- No run-time type information:

-- Let's try the example from the slides. 'fst' is predefined.

restrictedFst :: (Int, Int) -> Int
restrictedFst = fst

-- newFst :: (a, b) -> a
-- newFst = restrictedFst

-- Verify that 'newFst' produces a type error.

--------------------------------------------------------------------------
-- Example (Parametric polymorphism):

-- Try to implement *all* functions of type
--
--   (a, a) -> (a, a)

--------------------------------------------------------------------------
-- Several class constraints:

example :: (Eq a, Eq b, Show a) => (a, b) -> (a, b) -> String
example (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2  =  show x1
  | otherwise             =  "different"

-- Comment out the type signature, and verify that GHCi can infer the type.

--------------------------------------------------------------------------
-- 'Read':

-- Try the following examples in GHCi:
--
-- read "1" + 2
-- not (read "False")
-- [Just True, read "Just False"]

--------------------------------------------------------------------------
-- Unresolved overloading:

-- strange x = show (read x)
--
-- Uncomment the above definition and try to understand the error message.

--------------------------------------------------------------------------
-- Manually resolving overloading:

strange1 :: String -> String
strange1 x = show (read x :: Bool)

strange2 :: String -> String
strange2 x = show (read x :: Int)

-- Try in GHCi:
--
-- strange1 "1"
-- strange1 "False"
-- strange2 "1"
-- strange2 "False"

--------------------------------------------------------------------------
-- No automatic coercion:

-- Try these examples in GHCi, and also query their types:
--
-- 3 * 4
-- 3.2 * 4.5
-- 3.2 * (5 `div` 2)
-- 3.2 * fromIntegral (5 `div` 2)
-- 3.2 * 2

--------------------------------------------------------------------------
-- Use GHCi for information:

-- Try the :i command on a number of datatypes, functions and classes.

--------------------------------------------------------------------------
-- Function composition:

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

infixr 9 .  -- sets associativity and priority of the operator

-- Try in GHCi:
--
-- :t (.)

--------------------------------------------------------------------------
-- Composing functions:

oddSquares :: [Int]
oddSquares = (take 100 . filter odd . map (\ x -> x * x)) [1..]

--------------------------------------------------------------------------
-- Flipping a function:

foreach = flip map

-- Try to predict, then check in GHCi:
--
-- :t foreach
--
-- Also try:
--
-- (<=) 3 5
-- flip (<=) 3 5
-- flip (<=) 5 3

--------------------------------------------------------------------------
-- Currying and uncurrying:

-- Try the example in GHCi:
--
-- zip [1..3] [4..6]
-- :t (*)
-- :t uncurry (*)
-- map (uncurry (*)) (zip [1..3] [4..6])

--------------------------------------------------------------------------
-- From an informal pattern to a function (foldr):

foldr  ::  (a -> r -> r) -> r -> [a] -> r
foldr cons nil = go
  where
    go []        =  nil
    go (x : xs)  =  cons x (go xs)

--------------------------------------------------------------------------
-- Using 'foldr':

-- The function 'const' returns the first of two arguments.

const :: a -> b -> a
const x _ = x

length' :: [a] -> Int
length' = foldr (const (1+)) 0

-- Verify in GHCi:
--
-- :t const
-- :t (1+)
-- :t const (1+)
-- 
-- Try length'.

--------------------------------------------------------------------------
-- Accumulating parameter pattern:

reverse' :: [a] -> [a]
reverse' = go []
  where
    go :: [a] -> [a] -> [a]
    go  acc  []        =  acc
    go  acc  (x : xs)  =  go (x : acc) xs

sum' :: Num a => [a] -> a
sum' = go 0
  where
    go :: Num a => a -> [a] -> a
    go  acc  []        =  acc
    go  acc  (x : xs)  =  go (x + acc) xs

--------------------------------------------------------------------------
-- Abstracting (foldl):

foldl :: (r -> a -> r) -> r -> [a] -> r
foldl op e = go e
  where
    go  acc  []        =  acc
    go  acc  (x : xs)  =  go (op acc x) xs

-- Now we can write 'reverse' and 'sum' as an invocation of 'foldl':

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- Try to define a foldl-based version of 'sum' yourself.

--------------------------------------------------------------------------
-- Mapping over other types:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)   = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

--------------------------------------------------------------------------
-- The 'Functor' class:

instance Functor Tree where
  fmap = mapTree

aTree :: Tree Int
aTree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- Try in GHCi:
--
-- fmap (1+) aTree
-- fmap (*2) aTree
-- fmap (flip replicate 'x') aTree
-- fmap (+1) Nothing
-- fmap (+1) (Just 5)
-- (+1) <$> Just 5
-- length <$> ["well", "typed", "skills", "matter"]
-- compare 3 <$> aTree

--------------------------------------------------------------------------
-- Hiding names, selected imports, qualified imports:

-- Look at the top of this file and try to understand the 'import'
-- declarations listed there.

--------------------------------------------------------------------------
-- Finite maps:

-- Let's play a little with the 'Map' type. We have imported the
-- type itself directly in the module header, but all functions are
-- imported qualified with a prefix of 'M' to prevent clashes with
-- similarly named functions on lists.

-- We can easily build a map from an association list using 'fromList':

sampleMap :: Map Int String
sampleMap = M.fromList [(1, "one"), (2, "two"), (7, "seven")]

-- Now try in GHCi:
--
-- M.size sampleMap
-- M.elems sampleMap
-- M.keys sampleMap
-- sampleMap M.! 2
-- sampleMap M.! 4
-- M.lookup 2 sampleMap
-- M.lookup 4 sampleMap
-- M.insert 4 "four" sampleMap
--
-- Note that the last one produces a new finite map -- sampleMap is unchanged:
--
-- M.lookup 4 sampleMap
-- M.lookup 4 (M.insert 4 "four" sampleMap)
-- M.delete 1 sampleMap
-- fmap length sampleMap
-- length <$> sampleMap
-- M.mapKeys (+1) sampleMap
-- M.mapWithKey (\ n txt -> show n ++ " is " ++ txt) sampleMap


