{-# LANGUAGE DeriveGeneric #-}

module ApplicativeListExercise where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.Generics

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show, Generic)

take' :: Int -> List a -> List a
take' 0 xs = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Functor List where
  fmap f = fold (\x acc -> Cons (f x) acc) Nil
  -- non fmap implementation:
  -- fmap _ Nil = Nil
  -- fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- note "flatMap (`fmap` xs) fs" which translate
-- to flatMap (\f -> flatMap fmap f xs) fs
instance Applicative List where
  pure f = Cons f Nil
  fs <*> xs = flatMap (`fmap` xs) fs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f (Cons x xs) = f x `append` flatMap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = arrayToList <$> arbitrary

instance CoArbitrary (List a)

instance Eq a => EqProp (List a) where
  Nil =-= ys = ys `eq` Nil
  xs =-= Nil = xs `eq` Nil
  Cons x xs =-= Cons y ys = x `eq` y .&. xs `eq` ys

-- Nick helpers
arrayToList :: [a] -> List a
arrayToList = foldr Cons Nil


-- ZipList'

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure f = ZipList' (arrayToList $ repeat f)
  ZipList' fs <*> ZipList' xs = ZipList' $ zip' fs xs
    where
      zip' Nil _ = Nil
      zip' _ Nil = Nil
      zip' (Cons f fs) (Cons a as) = Cons (f a) (zip' fs as)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- testZipList' = quickBatch (applicative (undefined :: ZipList' (List String, List Int, List String)))
