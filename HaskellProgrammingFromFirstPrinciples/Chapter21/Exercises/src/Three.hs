module Three where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

--

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) =  f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

--

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
