module Three' where

import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid

data Three' a b = Three' a b b
  deriving (Eq, Ord, Show)

--

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b') =  f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

--

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
