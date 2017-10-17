{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a = 
  MaybeT { runMaybeT :: m (Maybe a) }
  
instance (Functor m) => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = 
    let
      mb = (fmap.fmap) f ma
    in
      MaybeT mb  


instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT (pure (pure a))
  
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT matob <*> MaybeT ma = 
   let
      -- matob :: m (Maybe (a -> b))
      -- ma :: m (Maybe a)
      -- x :: m (Maybe a -> Maybe b)
      x = (<*>) <$> matob -- OR fmap (<*>) matob
      b = x <*> ma
    in
      MaybeT b

-- finally got it.

x :: Show a => Maybe (a -> String)
x = (Just show)

-- by partially applying (<*>) it transforms:
--   Maybe (a -> String)
-- into
--   Maybe a -> Maybe String
-- ie waiting for a (Maybe a)
xx :: Show a => Maybe a -> Maybe String 
xx = ((<*>) (Just show))

-- so above definition of <*> for MaybeT, x is the form
-- that allows m's <*> to be used:
-- m (Maybe a -> Maybe b) <*> m (Maybe a)
--              x                ma

--
--
--

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      -- ma :: m (Maybe a)
      -- v :: Maybe a
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)
  