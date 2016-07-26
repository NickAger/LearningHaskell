{-# LANGUAGE InstanceSigs #-}

module MonadOfFunctions where
  
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

frooty :: (Foldable f, Functor f, Num a) => f a -> (f a, Int)
frooty r = bar (foo r) r

-- taken from: FunctionApplicative
newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

--(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)
--

-- 1.


instance Monad (Reader r) where
  return = pure
  
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) $ r
  