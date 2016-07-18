module ListApplicativeExercise where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 xs = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil = Nil

instance Applicative List where
  pure f = Cons f Nil
  (<*>) _ Nil = Nil
  (<*>) f'@(Cons f Nil) (Cons x xs) = Cons (f x) (f' <*> xs)

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
  arbitrary = do
    xs <- arbitrary
    return $ fold Cons xs Nil

instance Eq a => EqProp (List a) where
  Nil =-= ys = ys `eq` Nil
  xs =-= Nil = xs `eq` Nil
  Cons x xs =-= Cons y ys = x `eq` y .&. xs `eq` ys  

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)


instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l
