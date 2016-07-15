module InstancesOfFunctorExercises where
  
import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorLaws
  
-- Implement Functor instances for the following datatypes. Use the QuickCheck properties we just showed you to validate them.

-- Exercise 1:
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = Identity <$> arbitrary
  
type IdentityFC = Identity Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 2:
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
  
instance Arbitrary a => Arbitrary (Pair a) where
   arbitrary = Pair <$> arbitrary <*> arbitrary

type PairFC = Pair Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 3:
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
   arbitrary = Two <$> arbitrary <*> arbitrary
   
type TwoFC = Two String Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 4:
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
   arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
   
type ThreeFC = Three String String Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 5:
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
   arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
   
type ThreeFC' = Three' String Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 6:
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
   arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
   
type FourFC = Four String String String Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 7:
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
   arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
   
type FourFC' = Four' String Int -> Fun Int String -> Fun String Int -> Bool

-- Exercise 8:
-- Can you implement Functor instance for the following type. Why? Why not?
data Trivial = Trivial
-- Answer: it is not possible to implement a Functor for this type as the class functor is defined as:
-- "class Functor (f :: * -> *) where ..." so the Fuctor instance must be of a type with kind: * -> *. Trivial's kind is *



