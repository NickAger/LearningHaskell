module Exercises where
 
import Test.HUnit

-- Ex. 1
data Sum a b = 
    First b
  | Second a
  
instance Functor (Sum e) where
   fmap f (First a) = First (f a)
   fmap f (Second b) = Second b
   
-- Ex. 2
--data Company a b c =
--    DeepBlue a c
--  | Something b

data Company a b c =
    DeepBlue a b
  | Something c
  
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
  
-- Ex. 3

--data More a b = 
--    L a b a
--  | R b a b
--  deriving (Eq, Show)

data More a b = 
    R a b a
  | L b a b
  deriving (Eq, Show)
  
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
  
test1 = TestCase (assertEqual "" (fmap (+1) (L 1 2 3)) (L 2 2 4))
test2 = TestCase (assertEqual "" (fmap (+1) (R 1 2 3)) (R 1 3 3))

