module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import ArbitrarySum

-- 1. Identity
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 2. Pair
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x1 x2) = Pair (f x1) (f x2)

instance Applicative Pair where
  pure f = Pair f f
  (Pair f1 f2) <*> (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 3. Two
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x1 x2) = Two x1 (f x2)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two f1 f2) <*> (Two x1 x2) = Two (f1<>x1) (f2 x2)

instance (Arbitrary a, Arbitrary b)=> Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 4. Three
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x1 x2 x3) = Three x1 x2 (f x3)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three f1 f2 f3) <*> (Three x1 x2 x3) = Three (f1<>x1) (f2<>x2) (f3 x3)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 5. Three'
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' x1 x2 x3) = Three' x1 (f x2) (f x3)

instance (Monoid a) => Applicative (Three' a) where
  pure f = Three' mempty f f
  (Three' f1 f2 f3) <*> (Three' x1 x2 x3) = Three' (f1<>x1) (f2 x2) (f3 x3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 6. Four
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four x1 x2 x3 x4) = Four x1 x2 x3 (f x4)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four f1 f2 f3 f4) <*> (Four x1 x2 x3 x4) = Four (f1<>x1) (f2<>x2) (f3<>x3) (f4 x4)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

  -- 7. Four'
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 x4) = Four' x1 x2 x3 (f x4)
instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' f1 f2 f3 f4) <*> (Four' x1 x2 x3 x4) = Four' (f1<>x1) (f2<>x2) (f3<>x3) (f4 x4)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
--
main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Identity (Int, String, Int))
  quickBatch $ applicative (undefined :: Pair (Int, String, Int))
  quickBatch $ applicative (undefined :: Two String (Int, String, Int))
  quickBatch $ applicative (undefined :: Three String (Sum Int) (Int, String, Int))
  quickBatch $ applicative (undefined :: Four String (Sum Int) String (Int, String, Int))
  quickBatch $ applicative (undefined :: Four' String (Int, String, Int))
