module State where
  
import Control.Monad
import Test.QuickCheck

newtype State s a = MkState {unState :: s -> (a,s)}

instance Functor (State s) where
  fmap f (MkState x) = MkState (\s -> let (a, s') = x s in (f a, s'))

instance Applicative (State s) where
  pure x = MkState (\s -> (x,s))
  MkState f <*> MkState x = 
    MkState (\s -> 
      let
        (f', s') = f s
        (a, s'') = x s'
      in
        (f' a, s'')) 
  
instance Monad (State s) where
  return x = MkState (\s -> (x,s))
  MkState f >>= g = MkState (\s -> let (a,s') = f s in
                                    unState (g a) s')
                                    
get :: State s s
get = MkState (\s -> (s, s))

put :: s -> State s ()
put s = MkState (\_ -> ((), s))

(===) ::
  Eq a => State Integer a -> State Integer a -> Integer -> Bool
(f === g) s = unState f s == unState g s

prop_get_get =
  do x <- get
     y <- get
     return (x,y)
  State.===
  do x <- get
     return (x,x)
     
prop_put_put =
  do put 10
     put 20
     x <- get
     return x
  State.===
  do put 20 
     x <- get
     return x
     