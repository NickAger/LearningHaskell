import System.Random

-- extra experiments
-- reminding myself
-- how reader works
newtype Reader r a =Reader { runReader :: r -> a }


-- can think of state as being defined as:
-- State :: (s -> (a,s)) -> State s a
-- runState :: State s a -> s -> (a,s)

-- newtype State s a = State { runState :: s -> (a,s) }


