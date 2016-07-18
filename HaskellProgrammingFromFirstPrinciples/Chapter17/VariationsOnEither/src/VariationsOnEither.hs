module VariationsOnEither where

-- import Data.Monoid hiding (Sum, First)

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
  (<*>) = undefined
