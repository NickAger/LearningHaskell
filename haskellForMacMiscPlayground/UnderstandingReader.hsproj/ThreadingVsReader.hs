module ThreadingVsReader where

import Control.Monad.Reader

type Config = String
type SomeType = String

-- https://www.reddit.com/r/haskell/comments/2acj3w/threading_argument_vs_reader_monad/

-- When is it more convenient to use a Reader Monad as opposed to just threading an argument through your program?
-- I.e. what is the advantage of:
f :: String -> Reader Config SomeType
f = undefined

-- vs
g :: String -> Config -> SomeType
g = undefined

-- The consensus as I understand it seems to be (upstream/downstream refer to a methods call graph). Reader monad allows environment-independent methods from having to manage the environment for downstream dependencies.

-- ALSO

-- https://www.reddit.com/r/haskell/comments/4c9en3/when_to_prefer_a_reader_monad_over_a_function/

-- When to prefer a Reader monad over a function? 

-- ALSO

-- https://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad
-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-reader-monad
-- https://gist.github.com/egonSchiele/5752172
-- https://stackoverflow.com/questions/14178889/what-is-the-purpose-of-the-reader-monad/14206724#14206724
-- https://stackoverflow.com/questions/3451546/help-with-reader-monad
-- https://stackoverflow.com/questions/14178889/what-is-the-purpose-of-the-reader-monad

