module Monads where

import Control.Applicative
import Control.Monad hiding (foldM)
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.State hiding (foldM)
import Data.List hiding (permutations)
--import Data.List
import Test.QuickCheck

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the second part of the Patterns for Effects
-- slides, and contains additional exercises.
--
-- (For the first part, just listen.)

--------------------------------------------------------------------------
-- Evaluating example expressions:
--
-- Try the expressions from the slides:
--
-- replicateM 2 [1 .. 3]
-- mapM return [1 .. 3]
-- sequence [[1, 2], [3, 4], [5, 6]]
-- mapM (flip lookup [(1, ’x’), (2, ’y’), (3, ’z’)]) [1 .. 3]
-- mapM (flip lookup [(1, ’x’), (2, ’y’), (3, ’z’)]) [1, 4, 3]
-- evalState (replicateM_ 5 (modify (+ 2)) > get) 0

--------------------------------------------------------------------------
-- Experimenting with Maybe:

-- On the slides, there's this version of the factorial function:

-- fac :: Int -> Either String Int
-- fac 0             = Right 1
-- fac n | n > 0     = case fac (n - 1) of
--                       Left e  -> Left e
--                       Right r -> Right (n * r)
--       | otherwise = Left "fac: negative argument"

fac :: Int -> Either String Int
fac 0             = Right 1
fac n | n > 0     = do n' <- fac (n - 1); return (n * n')
      | otherwise = Left "fac: negative argument"

-- Rewrite this version using the Maybe monad.

-- Write a version of dfs (from More.hs) that does not rely
-- on the graph to be valid, and uses the Maybe monad.

--------------------------------------------------------------------------
-- Experimenting with Lists:

-- Write a function that given two lists, generates the cartesian product,
-- i.e., all combinations of elements of the two lists, using either do
-- notation on lists, or a list comprehension (preferably both).

cartesian :: [a] -> [b] -> [(a, b)]
cartesian x y = do
  x' <- x
  y' <- y
  return (x', y')

-- alernatives
--cartesian x y = (,) <$> x <*> y 

--cartesian x y = [ (x', y') | x' <- x, y' <- y ]

-- Write a function that generates all permutations of a given list;
-- again try using a list comprehension or do notation.

permutations :: Eq a => [a] -> [[a]] 
permutations [] = [[]]
permutations xs = [ x:ps | x <- xs , ps <- permutations ( filter (/=x) xs) ]

--------------------------------------------------------------------------
-- Experimenting with State:

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

-- The final version of 'labelTree':

labelTree :: Tree a -> State Int (Tree (a, Int))
labelTree (Leaf x) = do
  c <- get
  put (c + 1)
  return (Leaf (x, c))
labelTree (Node l r) = Node <$> labelTree l <*> labelTree r

labelTreeFrom0 :: Tree a -> Tree (a, Int)
labelTreeFrom0 t = evalState (labelTree t) 0

exTree :: Tree Int
exTree = Node (Leaf 1) (Node (Leaf 5) (Leaf 3))

-- Exercise:
--
-- Run this on one or more examples.
--
-- What if you'd instead want to number the tree from right to left?

data Rose a = Fork a [Rose a]
  deriving (Eq, Show)

labelRose :: Rose a -> State Int (Rose (a, Int))
labelRose (Fork x cs) = do
  c <- get
  put (c + 1)
  lcs <- mapM labelRose cs
  return (Fork (x, c) lcs)

-- Try to reimplement the sum function on lists using the
-- state monad, by applying mapM_ ...

-- Try to write a tree-labelling function that labels every
-- node with a label from a given supply, discarding the previous
-- labels. The function may crash if there are too few labels
-- in the supply.

labelTree' :: [a] -> Tree b -> Tree a
labelTree' = error "TODO: implement labelTree'"

-- Now, write a variant of this function that keeps the existing
-- label if it is a 'Just', and takes one from the supply if it is
-- 'Nothing'. The function may again crash if there are too few
-- labels in the supply.

completeLabels :: [a] -> Tree (Maybe a) -> Tree a
completeLabels = error "TODO: implement completeLabels"

--------------------------------------------------------------------------
-- Experimenting with IO:

-- One monadic function we have not yet discussed is the monadic
-- fold 'foldM':
--
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--
-- Compare this with the type of 'foldl'.
--
-- Here is an example:

verboseSum :: [Int] -> IO Int
verboseSum = foldM (\ acc x -> do
                        putStrLn $ "current total: " ++ show acc
                        return (acc + x))
                   0

-- Try it on a couple of lists and see how it works.
--
-- Then try to write your own version of 'foldM' that achieves the
-- same.

--------------------------------------------------------------------------
-- More about QuickCheck generators:

-- Let's try to define an arbitrary instance for binary trees.
--
-- We are going to make nodes a bit more frequent than leaves, in
-- order to get larger trees:

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [ (3, error "TODO: generate a node")
              , (1, error "TODO: generate a leaf") ]

-- Try to fill in the gaps. Note that one way to generate a node here
-- would be to generate a pair of subtrees and use fmap:
--
--   fmap (\ (l, r) -> Node l r) arbitrary
--
-- However, now that we know that Gen is a monad, it seems unnecessary
-- to first create a pair only to destruct it immediately after.
--
-- Instead, try to achieve the same using either do notation, or the
-- applicative notation (using (<$>) and (<*>)).

-- Then let's try
--
-- sample (arbitrary :: IO Int)
--
-- If you've done it right, you'll see trees being printed forever. It
-- seems that tree generation does not terminate. Any idea why?
--
--
-- The problem we're observing is quite typical for recursive branching
-- structures -- we've made nodes quite probable to be generated, and
-- each node has two new subtrees, so this quickly explodes. On the other
-- hand, if we make leaves really likely, we'll most of the time end up
-- with boring, extremely small trees.
--
-- Fortunately, QuickCheck generators have a size parameter. This
-- parameter is normally hidden state in the 'Gen' monad. However, we
-- can access it using the 'sized' function:
--
-- sized :: (Int -> Gen a) -> Gen a
--
-- We use it as follows:
--
-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary = sized $ \ s -> ...
--
-- Now we have an additional parameter 's' available. We can also
-- "resize" an existing generator:
--
-- resize :: Int -> Gen a -> Gen a
--
-- Now, rewrite arbitrary for binary trees such that:
--
--   * if the size is 0, we always generate a leave
--   * if the size is non-zero, we generate the old frequency table,
--     but we resize the subtrees to one half of the input size.
--
-- Sample again in order to check if this works.
--
-- Test the following property:
--
--   The size of a tree is equal to the length of the flattened tree.
--
-- Next, try to write a generator for node-labeled Trees ('BinTree'
-- from More.hs), using the same techniques.
--
-- As a final challenge, try to write a generator for BSTs (binary
-- search trees). When you're done, check whether your generated BSTs
-- actually have the BST property.
