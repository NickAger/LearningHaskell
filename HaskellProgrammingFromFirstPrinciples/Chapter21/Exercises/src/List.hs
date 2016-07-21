module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

--

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) =  f x <> foldMap f xs

-- traverse (\x -> [x * x])   (Cons 1 (Cons 2 Nil))
instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

--

arrayToList :: [a] -> List a
arrayToList = foldr Cons Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = arrayToList <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq
