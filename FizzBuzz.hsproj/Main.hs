import Control.Monad.State
import Data.Functor
import Control.Monad.Writer.Lazy

fizzBuzz :: Integer -> String
fizzBuzz n
  | (n `rem` 15 == 0) = "FizzBuzz"
  | (n `rem` 3 == 0) = "Fizz"
  | (n `rem` 5 == 0) = "Buzz"
  | otherwise = show n

-- from http://stackoverflow.com/questions/6957260/fizzbuzz-in-haskell

fizzBuzz2 :: Integer -> String
fizzBuzz2 x =
  case (x `rem` 3, x `rem` 5) of
    (0,0) -> "fizzbuzz"
    (0,_) -> "fizz"
    (_,0) -> "buzz"
    _     -> show x


fizzBuzz3 :: Integer -> String
fizzBuzz3 x = max(show x)(concat[n|(f,n)<-[(3,"Fizz"),(5,"Buzz")],rem x f==0])    

-- modify to make clear how many `rem` are being used each time, using a state monad to record each use.

type FizzBuzzState = State Int String

-- `> snd $ runState (mapM fizzBuzzState [1..100]) 0`
-- `47`
fizzBuzzState :: Integer -> FizzBuzzState
fizzBuzzState n
  | (n `rem` 15 == 0) = state $ \s -> ("FizzBuzz", s + 1) 
  | (n `rem` 3 == 0) = state $ \s -> ("Fizz", s + 2)
  | (n `rem` 5 == 0) = state $ \s -> ("Buzz", s + 3)
  | otherwise = modify (+3) $> show n

-- `> snd $ runState (mapM fizzBuzz2State [1..100]) 0`
-- `200`
fizzBuzz2State :: Integer -> FizzBuzzState
fizzBuzz2State x =
  case (x `rem` 3, x `rem` 5) of
    (0,0) -> state $ \s -> ("FizzBuzz", s + 2)
    (0,_) -> state $ \s -> ("Fizz", s + 2)
    (_,0) -> state $ \s -> ("Buzz", s + 2)
    _     -> state $ \s -> (show x, s + 2)
    

-- `> snd $ runState (mapM fizzBuzz3State [1..100]) 0`
-- `53`
fizzBuzz3State :: Integer -> FizzBuzzState
fizzBuzz3State x = 
  let
    rslts  = ([n|(f,n)<-[(3,state $ \s -> ("Fizz", s + 1)),(5,state $ \s -> ("Buzz", s + 1))],rem x f==0]) :: [FizzBuzzState]
  in
    if (null rslts) then (return $ show x) else (combine rslts)
  where
    combine :: [FizzBuzzState] -> FizzBuzzState
    combine [r] = r
    combine [r1, r2] = 
      state $ \s -> 
        let 
          (a, s') = runState r1 s
          (a', s'') = runState r2 s'
        in
          (a ++ a', s'')
          
type RemCount = Writer (Sum Int)

fizzBuzz4State :: RemCount [String]
fizzBuzz4State = foldM folder [] [1..100]
  where
  folder :: [String] -> Integer -> RemCount [String]
  folder acc n
    | n `rem` 15 == 0 = writer ("FizzBuzz":acc,Sum 1)
    | n `rem` 3 == 0 = writer ("Fizz":acc, Sum 1)
    | n `rem` 5 == 0 = writer ("Buzz":acc, Sum 1)
    | otherwise = writer ("":acc, Sum 0)