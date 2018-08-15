{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.Functor
import Control.Monad.Writer.Lazy
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Either
import Control.Monad.Trans.Either
import qualified Data.IntMap as M
import Control.Arrow (first, second)

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

--

fizzBuzz3 :: Integer -> String
fizzBuzz3 x = max(show x)(concat[n|(f,n)<-[(3,"Fizz"),(5,"Buzz")],rem x f==0])  

--

fizzes :: Int -> M.IntMap String
fizzes n = foldr (\x acc-> M.insert x "Fizz" acc) M.empty [3,6..n]

buzzes :: Int -> M.IntMap String -> M.IntMap String
buzzes n m = foldr (\x acc-> M.insertWith (flip (++)) x "Buzz" acc) m [5,10..n]

fizzBuzz4 n = do
    x <- [1..n]
    case M.lookup x fbmap of
        Nothing -> show x ++ "\n"
        (Just fb) -> fb ++ "\n"
    where
        fbmap = buzzes n $ fizzes n  
        
--

data FizzBuzz
  = Number Int
  | Fizz
  | Buzz
  | FizzBuzz
  deriving (Show)

fizzBuzz5 :: [FizzBuzz]
fizzBuzz5 = fmap buzzify [1..]
  where
    buzzify n
      | isFizz n && isBuzz n = FizzBuzz
      | isFizz n             = Fizz
      | isBuzz n             = Buzz
      | otherwise            = Number n

    isFizz n = n `rem` 3 == 0
    isBuzz n = n `rem` 5 == 0

-- modify to make clear how many `rem` are being used each time, using a state monad to record each use.

type FizzBuzzState = State Int String

-- `> snd $ runState (mapM fizzBuzzState [1..100]) 0`
-- `261`
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
    

remCount :: MonadWriter (Sum Int) m => Integer -> Integer -> m Integer
n `remCount` d = writer (n `rem` d, Sum 1)

fizzBuzz6State :: Integer -> RemCount String
fizzBuzz6State n = do
  r1 <- n `remCount` 15
  if r1 == 0 
    then return "FizzBuzz"
    else do
      r2 <-  n `remCount` 3
      if r2 == 0
        then return "Fizz"
        else do
          r3 <- n `remCount` 5
          if r3 == 0 
            then return "Buzz"
            else return $ show n
    
-- lazy evaluation doesn't seem to work here, I guess because of desugaring into bind operations.       
fizzBuzz7State :: Integer -> RemCount String
fizzBuzz7State n = do
  r1 <- n `remCount` 15
  r2 <- n `remCount` 3
  r3 <- n `remCount` 5
  
  let rslts = [(r1, "FizzBuzz"), (r2, "Fizz"), (r3, "Buzz"), (0, show n)]
  return $ snd $ fromJust $ find ((==0).fst) rslts

--fizzBuzz8State :: Integer -> RemCount String
--fizzBuzz8State n = let
--    rslts = [(n `remCount` 15, "FizzBuzz"), (n `remCount` 3, "Fizz"), (n `remCount` 5, "Buzz"), (pure 0, show n)]
--    
--  in
--    
--  return $ snd $ fromJust $ find ((==0).fst) rslts 

-- > snd $ runWriter (mapM fizzBuzz6State [1..100]) 
-- Sum {getSum = 261}
fizzBuzz9State :: Integer -> RemCount String
fizzBuzz9State n = fmap (either id id) $ runEitherT $ do
  r1 <- n `remCount` 15
  when (r1 == 0) $ left "FizzBuzz"
  r2 <-  n `remCount` 3
  when (r2 == 0) $ left "Fizz"
  r3 <- n `remCount` 5
  when (r3 == 0) $ left "Buzz"
  return $ show n

main :: IO ()
main = do
  let (text, count) = first unlines $ runWriter ([1..100] `forM` fizzBuzz9State)
  putStrLn text 
 