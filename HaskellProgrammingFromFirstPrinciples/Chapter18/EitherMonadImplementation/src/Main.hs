module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First e) = First e
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  (First e) <*> _ = First e
  _ <*> (First e) = First e
  (Second f) <*> (Second x) = Second $ f x

instance Monad (Sum a) where
  return = pure
  (First e) >>= _ = First e
  (Second x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (First e1) =-= (First e2) = e1 `eq` e2
  (Second x1) =-= (Second x2) = x1 `eq` x2
  (=-=) _ _ = property False

-- monad is defined - https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs
-- monad :: {-lots of stuff)-} => m (a,b,c) -> TestBatch

-- couldn't get this to work
main :: IO ()
main = do
  quickBatch (monad (Second 3))
