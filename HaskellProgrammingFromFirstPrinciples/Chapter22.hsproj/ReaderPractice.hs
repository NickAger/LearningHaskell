-- section 22.8

module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 a the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- one that returns Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z


x1 :: Maybe (Integer, Integer)
-- x1 = (,) <$> xs <*> ys
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)


main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  
-- 1.
  print $ foldr (&&) True $ sequA 3
 
-- 2.
  print $ sequA $ fromMaybe 0 s'
  
-- 3.
  print $ bolt $ fromMaybe 0 ys
  
-- 4.
  print $ bolt $ fromMaybe 0 $ z' 3
  