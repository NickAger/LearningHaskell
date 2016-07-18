{-# OPTIONS_GHC -Wall #-}

module VariationsOnEither where

import Test.QuickCheck hiding (Success)
import Data.Monoid hiding (Sum, First)
import Test.QuickCheck.Checkers

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (First x) <*> _ = First x
  _ <*> (First x) = First x
  (Second a) <*> (Second b) = Second (a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  First a1 =-= First a2 = a1 `eq` a2
  Second b1 =-= Second b2 = b1 `eq` b2
  _ =-= _ = property False

--

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error x) = Error x
  fmap f (Success x) = Success (f x)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Error e1) <*> (Error e2) = Error (e1 <> e2)
  (Error x) <*> _ = Error x
  _ <*> (Error x) = Error x
  (Success a) <*> (Success b) = Success (a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Error <$> arbitrary, Success <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  Error a1 =-= Error a2 = a1 `eq` a2
  Success b1 =-= Success b2 = b1 `eq` b2
  _ =-= _ = property False
