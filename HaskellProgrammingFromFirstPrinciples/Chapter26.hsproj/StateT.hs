{-# LANGUAGE InstanceSigs #-}
module StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

-- first failed attempt
--instance (Functor m) => Functor (StateT s m) where
--  fmap :: (a -> b) -> StateT s m a -> StateT s m b
--  fmap f stom = StateT $ \s ->
--    let
--      t = (runStateT stom) s
--      a' = fmap fst t
--      s' = fmap snd t
--      b = fmap f a'
--    in
--      !!(m b, m s) rather than m (b, s)!!
--      (b,  s')
--      


-- the trick was to create a lambda that we can use
-- fmap to transform mas :: m (a, s) into m (b, s), ie
--    fmap lambda mas
-- where lambda :: m (a, s) -> m (b, s)
instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT stomas) = StateT $ \s ->      
    let
      mas = stomas s 
    in
      fmap (\(a, s) -> (f a, s)) mas
      

-- Applicative
-- note the (Monad m) constraint
instance (Monad m) => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> pure (a, s)
    
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT f) <*> (StateT stomas) = StateT $ \s ->
      do
        (atob, s') <- f s
        (a, s'') <- stomas s'
        return (atob a, s'')
        

-- Monad    
instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT stomas) >>= atoST = StateT $ \s ->
      do
        (a, s') <- stomas s
        let stombs = runStateT $ atoST a
        (b, s'') <- stombs s'
        return (b, s'')
    

