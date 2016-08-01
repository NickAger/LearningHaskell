-- 23.6 Write State for yourself

module MoiState where
  
newtype Moi s a = 
  Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
--    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi helper
      where
        helper s =
          let (a, s') = g s
          in
             (f a, s')


instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  --(<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi helper
    where 
      helper s = 
        let (fab, s') = f s
            (a, _) = g s
        in
           (fab a, s')
           
-- Question:
-- I have choosen to ignore the state resulting in (g s) rather than ignoring the state from (f s) 
-- is one is prefered over the other or
-- both are equally good?
-- ... hmm if s is a monoid then we could combine them. 


instance Monad (Moi s) where
  return = pure
  
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi helper
    where
      helper s =
        let (a, s') = f s
        in (runMoi (g a)) s'  

  