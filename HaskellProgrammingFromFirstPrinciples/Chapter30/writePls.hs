module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
    print (typeOf e)
    putStrLn ("We errored! It was: " ++ show e)


-- didn't compile: "Couldn't match expected type ‘e’ with actual type ‘AsyncException’"
-- handler :: SomeException -> IO ()
-- handler (SomeException StackOverflow) = putStrLn "We errored! It was: StackOverflow"
-- handler (SomeException _) = putStrLn "We errored! It wasn't: StackOverflow"

-- compiles but doesn't catch the exception.
-- handler :: AsyncException -> IO ()
-- handler StackOverflow = putStrLn "We errored!, StackOverflow"
-- handler _ = putStrLn "We errored!, something other than StackOverflow"

    

main = do
    writeFile "zzz" "hi" `catch` handler
    putStrLn "wrote to a file"
    