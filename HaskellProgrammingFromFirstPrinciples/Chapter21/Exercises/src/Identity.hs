module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

--

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

--

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
