module Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)


instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node t1 x t2) = foldMap f t1 <> f x <> foldMap f t2

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node t1 x t2) = Node <$> traverse f t1 <*> f x <*> traverse f t2

--

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  x <- arbitrary
  t1 <- genTree
  t2 <- genTree
  frequency [ (1, return Empty)
            , (2, return $ Leaf x)
            , (2, return $ Node t1 x t2)
            ]

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = genTree

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq
