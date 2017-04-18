module Reader where
  
import Control.Monad

newtype Reader s a = MkReader {unReader :: s -> a}

instance Functor (Reader s) where
  fmap f (MkReader x) = MkReader (\s -> let a = x s in f a)
  
instance Applicative (Reader s) where
  pure x = MkReader (\s -> x)
  MkReader f <*> MkReader x = 
    MkReader (\s -> 
      let
        f' = f s
        a = x s
      in
        f' a)
        
instance Monad (Reader s) where
  MkReader f >>= g = MkReader (\s -> let a = f s in unReader (g a) s)

ask :: Reader s s
ask = MkReader (\s -> s)