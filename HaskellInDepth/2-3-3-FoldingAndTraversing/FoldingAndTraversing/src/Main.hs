module Main where

import           Control.Monad.Writer

main :: IO ()
main = do
  putStrLn "hello world"

-- test with:
-- ghci> traverse addNumber [1..5]
addNumber :: Int -> IO String
addNumber n = pure (++) <*> pure (show n ++ " ") <*> getLine

-- "Try to use the same approach for `sequenceA`: choose any container (take a list as
-- the simplest one; `Maybe` works too) and any applicative context (try `Writer`!) and then 
-- construct a call to `sequenceA`. You will definitely see what is going on there."

sumN :: Int -> Writer String Int
sumN 0 = writer (0, "finish. ")
sumN n = do
  tell (show n ++ ",")
  s <- sumN (n - 1)
  return (n + s)

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- test with:
--  ghci> runWriter $ sequenceA [sumN 4, sumN 2] 
--  ([10,3],"4,3,2,1,finish. 2,1,finish. ")
