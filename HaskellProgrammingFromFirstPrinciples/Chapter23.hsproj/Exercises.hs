module Exercises where
  
import Control.Monad.Trans.State hiding (get, put, modify)

-- reminder, State isn't defined like this but it will
-- do as an approximation... 
-- ... a major difference is that the State constructor
-- is not exposed.
--
-- newtype State s a = State { runState :: s -> (a,s) }

-- 1.
get :: State s s
get = state $ \s -> (s, s)

-- note use the `state` function not the `State` constructor
-- state :: Monad m => (s -> (a,s)) -> StateT s m a

-- 2.
put :: s -> State s ()
put s = state $ \_ -> ((), s)

-- 3. 
exec :: State s a -> s -> s
exec stateSA s = snd $ (runState stateSA) s

-- 4. 
eval :: State s a -> s -> a
eval stateSA s = fst $ (runState stateSA) s

-- 5.
modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)