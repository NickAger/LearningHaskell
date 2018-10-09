-- from https://arow.info/blog/posts/2015-07-10-servant-intro.html

{-# LANGUAGE TypeOperators #-}

data path :> a

data a :<|> b = a :<|> b

data Foo path a

{-
 the above are types which don't have constructors
 
 see http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html 
-}

data Zero
data Succ a

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ (Succ (Succ (Succ Zero)))

