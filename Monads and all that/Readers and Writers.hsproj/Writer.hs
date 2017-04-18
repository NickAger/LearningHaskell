module Writer where
  
import Control.Monad

data Writer s a = MkWriter [s] a

instance Functor (Writer s) where
  fmap f (MkWriter ss x) = MkWriter ss (f x)
  
instance Applicative (Writer s) where
  pure x = MkWriter [] x
  MkWriter ss f <*> MkWriter ss' x = 
    MkWriter (ss ++ ss') (f x) 
        
instance Monad (Writer s) where
  (MkWriter ss x) >>= g = let (MkWriter ss' x') = g x in MkWriter (ss ++ ss') x'

tell :: s -> Writer s ()
tell s = MkWriter [s] ()
