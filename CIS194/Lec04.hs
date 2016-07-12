{-# LANGUAGE MultiParamTypeClasses #-}
module Lec04 where

import Data.Functor
import Data.Monoid
{-
class Eq a where
    (==) :: a -> a -> Bool
    x == y = not (x /= y)
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
-}

data Foo = F Int
         | G Bool
           deriving (Show)

instance Eq Foo where
    F x == F y = x == y
    G x == G y = x == y
    _ == _ = False

f :: String -> String
f = show . (read :: String -> Int)

class Castable a b where
    cast :: a -> b

instance Castable Int Integer where
    cast = fromIntegral

maybeConcat :: Maybe String -> Maybe String -> Maybe String
maybeConcat (Just s1) (Just s2) = Just $ s1 ++ s2
maybeConcat Nothing (Just s)    = Just s
maybeConcat (Just s) Nothing    = Just s
maybeConcat Nothing Nothing     = Nothing

data X a = X a
           deriving (Show)

instance Functor X where
    fmap f (X a) = X $ f a

-- this is not the most efficient!
intInts :: Monoid m => (Integer -> m) -> m   -- interesting ints!
intInts mk_m = go [1..100]   -- [1..100] is the list of numbers from 1 to 100
  where go [] = mempty
        go (n:ns)
          | let div_by_5 = n `mod` 5 == 0
                div_by_7 = n `mod` 7 == 0
          , (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7))
          = mk_m n <> go ns
          | otherwise
          = go ns

intIntsList :: [Integer]
intIntsList = intInts (:[])

-- Style Examples -------------------------------------

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [x] = True
isSorted (x1 : x2 : xs)
    | x1 <= x2  = isSorted (x2 : xs)
    | otherwise = False

-- The original function had:
-- * Redundant base cases
-- * Unnecessary guards

isSorted' :: Ord a => [a] -> Bool
isSorted' (x1 : x2 : xs) = x1 <= x2 && isSorted' (x2 : xs)
isSorted' _ = True


countMistakes :: Eq a => [a] -> [[a]] -> [Int]
countMistakes ans []     = []
countMistakes ans (r:rs) =
    countMs ans r : countMistakes ans rs
    where countMs [] _  = 0
          countMs _  [] = 0
          countMs (x:xs) (y:ys)
              | x == y    = countMs xs ys
              | otherwise = 1 + countMs xs ys

-- The original version had:
-- * Explicit recursion patterns that can be replaced with `map` and `zipWith`

countMistakes' :: Eq a => [a] -> [[a]] -> [Int]
countMistakes' ans = map countMs
--  where countMs = sum . zipWith (\x y -> fromEnum (x/=y)) ans
    where countMs = sum . zipWith (fromEnum .: (/=)) ans
          (.:) = fmap.fmap 
-- let (.:) = fmap.fmap in (negate .: (*)) 4 3
