module BiFunctors where
  
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Deux a b = Deux a b

instance  Bifunctor Deux where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 f2 (Deux a c) = Deux (f1 a) (f2 c)
  
-- 2 
data Const a b = Const a

instance  Bifunctor Const where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 _ (Const a) = Const (f1 a)
  
-- 3
data Drei a b c = Drei a b c
instance  Bifunctor (Drei a) where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 f2 (Drei a b c) = Drei a (f1 b) (f2 c)
  
-- 4
data SuperDrei a b c = SuperDrei a b
instance  Bifunctor (SuperDrei a) where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 _ (SuperDrei a b) = SuperDrei a (f1 b)
  
-- 5
data SemiDrei a b c = SemiDrei a
instance  Bifunctor (SemiDrei a) where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap _ _ (SemiDrei a) = SemiDrei a
  
-- 6
data Quadriceps a b c d = Quadzzz a b c d
instance  Bifunctor (Quadriceps a b) where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 f2 (Quadzzz a b c d) = Quadzzz a  b (f1 c) (f2 d)
  
-- 7. 
-- data Either a b = Left a | Right b
instance  Bifunctor Either where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f1 _ (Left a) = Left (f1 a)
  bimap _ f2 (Right b) = Right (f2 b)
