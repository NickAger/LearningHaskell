{-# LANGUAGE InstanceSigs #-}

module ReaderT where
  
newtype ReaderT r m a = 
  ReaderT { runReaderT :: r -> m a }
  
instance (Functor m) => Functor (ReaderT  r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rtoma) = 
    ReaderT $ (fmap f) . rtoma
    
-- interestingly the book says:
--    ReaderT $ (fmap . fmap) f rtoma
-- I think my solution is more intention revealing.

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
--  pure a = ReaderT $ (\ _ -> pure a)
  pure a = ReaderT $ const (pure a)
  
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rtomatob) <*> (ReaderT rtoma) = 
    ReaderT (\r -> (rtomatob r) <*> (rtoma r))

-- The book solution is:
-- pure a = ReaderT (pure (pure a))
-- (ReaderT fmab) <*> (ReaderT rma) =
--   ReaderT $ (<*>) <$> fmab <*> rma
-- which matches the implementation of Either and Maybe


instance (Monad m) => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure
  
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = 
    ReaderT $ \r ->
      do
        a <- rma r
        let h = runReaderT (f a) r
        runReaderT (f a) r    
