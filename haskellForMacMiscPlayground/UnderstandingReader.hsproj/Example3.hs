-- https://passy.svbtle.com/dont-fear-the-reader
module Example3 where
  
import Control.Monad.Reader
  
example :: String
example = runReader computation "Hello"
    where
        computation :: Reader String String
        computation = do
            greeting <- ask
            return $ greeting ++ ", Haskell"




example1 :: String -> String
example1 context = runReader (computation "Tom") context
    where
        computation :: String -> Reader String String
        computation name = do
            greeting <- ask
            return $ greeting ++ ", " ++ name


example2 :: String -> String
example2 context = runReader (greet "James" >>= end) context
    where
        greet :: String -> Reader String String
        greet name = do
            greeting <- ask
            return $ greeting ++ ", " ++ name

        end :: String -> Reader String String
        end input = do
            isHello <- asks (== "Hello")
            return $ input ++ if isHello then "!" else "."
            
