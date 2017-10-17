{-# LANGUAGE InstanceSigs #-}
module MonadTransformers where


newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)
  
-- compare with:

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)
  
--- Functor instances

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
-- alternative:
--  fmap f identity = Identity (f (runIdentity identity))

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)  
  
--- Applicative instances

instance Applicative Identity where
  pure a = Identity a
  (Identity atob) <*> (Identity a) = Identity (atob a)
  
instance (Applicative m) => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (IdentityT fatob) <*> (IdentityT fa) = IdentityT (fatob <*> fa)  
  
-- I found it helped to think of (IdentityT m) as a concrete type such as (IdentityT Maybe)
-- then:
--   (IdentityT fatob) <*> (IdentityT fa) 
-- is equivalent to:
--   (IdentityT (Just (+1))) <*> (IdentityT (Just 2)) 

-- Monad Instances

instance Monad Identity where
  return = pure
  (Identity a) >>= atomb = atomb a
 
instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= fatomb = IdentityT (ma >>= runIdentityT . fatomb)


-- testIdentityT a = IdentityT (Just (show a))
-- (IdentityT (Just 1)) >>= testIdentityT = (IdentityT (Just "1")) 



      
  


