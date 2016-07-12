module Main where

import Data.List (permutations)
import Test.QuickCheck
import Test.QuickCheck.Modifiers

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the part on testing.
--
-- Only the original version of the code, and some of the properties are
-- provided. You can perform the changes yourself.

--------------------------------------------------------------------------
-- Insertion sort code:

sort :: [Int] -> [Int] -- not correct
sort []      =  []
sort (x:xs)  =  insert x xs

insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : ys
                 | otherwise    =  y : insert x ys

--------------------------------------------------------------------------
-- Properties:

sortPreservesLength :: [Int] -> Bool
sortPreservesLength xs = length xs == length (sort xs)

preserves :: Eq a => (t -> t) -> (t -> a) -> t -> Bool
(f `preserves` p) x = p x == p (f x)

sortPreservesLength' :: [Int] -> Bool
sortPreservesLength' = sort `preserves` length

idPreservesLength :: [Int] -> Bool
idPreservesLength = id `preserves` length

--------------------------------------------------------------------------
-- Specifying sortedness:

sorted :: [Int] -> Bool  -- not correct
sorted []            =  True
sorted [x]           =  True
sorted (x : y : ys)  =  x < y && sorted (y : ys)

(f `ensures` p) x = p (f x)

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted = sort `ensures` sorted

f `permutes` xs  =  f xs `elem` permutations xs

sortPermutes :: [Int] -> Bool
sortPermutes xs  =  sort `permutes` xs

--------------------------------------------------------------------------
-- Other properties:

appendLength :: [a] -> [a] -> Bool
appendLength xs ys = length xs + length ys == length (xs ++ ys)

plusIsCommutative :: Int -> Int -> Bool
plusIsCommutative m n = m + n == n + m

takeDrop :: Int -> [Int] -> Bool
takeDrop n xs = take n xs ++ drop n xs == xs

dropTwice :: Int -> Int -> [Int] -> Bool
dropTwice m n xs = drop m (drop n xs) == drop (m + n) xs

lengthEmpty :: Bool
lengthEmpty = length [] == 0

wrong :: Bool
wrong = False

--------------------------------------------------------------------------
-- Generators:

-- Try in GHCi:
--
-- sample (arbitrary :: IO Int)
-- sample (arbitrary :: IO [Int])
-- sample (arbitrary :: IO Bool)

mkSorted :: [Int] -> [Int]
mkSorted []           = []
mkSorted [x]          = [x]
mkSorted (x : y : ys) = x : mkSorted (x + abs y : ys)

-- Can you see a potential problem with mkSorted?

genSorted :: Gen [Int]
genSorted = fmap mkSorted arbitrary

insertPreservesSorted :: Int -> Property
insertPreservesSorted x = forAll genSorted (\ xs -> sorted xs ==> sorted (insert x xs))

insertPreservesSorted' :: Int -> OrderedList Int -> Bool
insertPreservesSorted' x (Ordered xs) = sorted (insert x xs)

--------------------------------------------------------------------------
-- Haskell Program Coverage:

-- We define a main program and run some tests, so that you can check
-- coverage.
main :: IO ()
main = do
  quickCheck dropTwice
  quickCheck sortPreservesLength
  quickCheck insertPreservesSorted'
