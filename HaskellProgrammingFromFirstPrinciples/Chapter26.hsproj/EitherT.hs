{-# LANGUAGE InstanceSigs #-}
module EitherT where
  
newtype EitherT e m a = 
  EitherT { runEitherT :: m (Either e a) }
  
instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mEea) = 
    let
      mEeb = (fmap . fmap) f mEea
    in
      EitherT mEeb
      
--

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure (pure a)
  
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT mEeatob <*> EitherT mEea = 
    EitherT $ fmap (<*>) mEeatob <*> mEea
  

--


instance Monad m => Monad (EitherT e m) where
  return = pure
  
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mEea >>= atoeT = 
    EitherT $ do
      eea <- mEea
      case eea of
        Left e -> return (Left e)
        Right a -> runEitherT (atoeT a)
        

--

-- 4. transformer version of swapEither
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mEea) = 
  EitherT $ fmap swapEither mEea

  where
    swapEither :: Either e a -> Either a e
    swapEither (Left e) = Right e
    swapEither (Right a) = Left a
    

-- 5. Write the transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT atomc btomc (EitherT amb) = do
  eab <- amb
  case eab of 
    Left a -> atomc a
    Right b -> btomc b
    

  
