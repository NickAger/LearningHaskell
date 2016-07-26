{-# LANGUAGE InstanceSigs #-}

module FunctionApplicative where
  
import Control.Applicative (liftA2)
  
newtype HumanName = 
  HumanName String
  deriving (Eq, Show)
  
newtype DogName =
  DogName String
  deriving (Eq, Show)
  
newtype Address =
  Address String
  deriving (Eq, Show)
  
data Person = 
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
  
data Dog =
  Dog {
    dogsname :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)
  
pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")
         

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")
               

-- without Reader
getDog :: Person -> Dog
getDog p = 
  Dog (dogName p) (address p)
  
-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address
  
-- with Reader alternative

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address
  
-- 1. 
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- 2.
newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3.
-- (.) ::(b->c)->(a->b)->(a->c)

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  
--(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)
  

-- 4.
getDogR'' :: Reader Person Dog
getDogR'' = Reader $ liftA2 Dog dogName address




