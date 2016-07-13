import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

half x = x `div` 2

-- applicatives
halfv2 x = if even x
          then Just (x `div` 2)
          else Nothing
  
-- Writer monad        
halfWithWriter :: Int -> Writer String Int
halfWithWriter x = do
                tell ("I just halved " ++ (show x) ++ "!")
                return (x `div` 2)
                

-- Writer monad take 2 from
-- http://learnyouahaskell.com/for-a-few-monads-more#writer

logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5 
    tell ["Gonna multiply these two"] 
    return (a*b)
    

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  


-- Reader monad
greeter :: Reader String String
greeter = do
    name <- ask
    return ("hello, " ++ name ++ "!")
    


-- State monad
greeter2 :: State String String
greeter2 = do
    name <- get
    put "tintin"
    return ("hello, " ++ name ++ "!")