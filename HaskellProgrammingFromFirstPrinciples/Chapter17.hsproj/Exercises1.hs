module Exercises1 where

import Control.Applicative
import Data.List (elemIndex)

-- Ex 1:
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3,4] [4,5,6])

-- Ex 2:
y' :: Maybe Integer
y' = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3,4][4,5,6]

tupled :: Maybe(Integer, Integer)
tupled = (,) <$> y' <*> z

-- Ex 3:
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y :: Maybe Int
y = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x y

-- Ex 4:
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

