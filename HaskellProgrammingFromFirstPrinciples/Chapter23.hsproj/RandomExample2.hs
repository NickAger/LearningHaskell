module RandomExample2 where
  
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- Six-sided die

data Die = 
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)
  
intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x
    
-- newtype State s a = State { runState :: s -> (a, s) }
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- randomR :: RandomGen g => (a, a) -> g -> (a, g)


-- without State Monad    
{-
rollDie'' :: StdGen -> (Die, StdGen)
rollDie'' a = 
  let (n, s) = randomR (1, 6) a
  in (intToDie n, s)
-}

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' = 
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie
  
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where 
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

-- 1.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where 
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen
        
-- 2. 

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where 
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum tuple@(count, dice) gen
      | sum >= n = tuple
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
            die' = intToDie die
        in go (sum + die) ((count + 1), die':dice) nextGen

