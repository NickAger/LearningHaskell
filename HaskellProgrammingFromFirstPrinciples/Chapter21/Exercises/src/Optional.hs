module Optional where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

--

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

--

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq
