module Main where

import Prelude hiding (Left, Right)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1. Nope
data Nope a =
  NopeDataJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDataJpg

instance Applicative Nope where
  pure _ = NopeDataJpg
  _ <*> _ = NopeDataJpg

instance Monad Nope where
  return _ = NopeDataJpg
  _ >>= _ = NopeDataJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDataJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

-- 2. PhhhbbtttEither
data PhhhbbtttEither b a =
    Left a
  | Right b
    deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left x) = Left $ f x
  fmap f (Right e) = Right e

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  Right e <*> _ = Right e
  _ <*> Right e = Right e
  Left f <*> Left x = Left $ f x

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right e >>= _ = Right e
  Left x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- 3. Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 4. List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil as' = as'
append (Cons a as) as' = Cons a $ as `append` as'

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x `append` (xs >>= f)

arrayToList :: [a] -> List a
arrayToList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = arrayToList <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq`  ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

--

testAll trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

main :: IO ()
main = do
  testAll (undefined :: Nope (Int, String, Int))
  testAll (undefined :: PhhhbbtttEither String (Int, String, Int))
  testAll (undefined :: Identity (Int, String, Int))
  testAll (undefined :: List (Int, String, Int))
