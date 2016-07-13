-- <nickager>  I’m struggling with understanding functional composition.
-- <nickager>  Why does:
-- <nickager>  (\x y -> negate $ x * y) 3 4
-- <nickager>  work
-- <nickager>  but
-- <nickager>  (negate . (*)) 4 3
-- <nickager>  gives an error:
-- <nickager>  No instance for (Num (a0 -> a0)) arising from a use of ‘it’
-- <nickager>  In a stmt of an interactive GHCi command: print it
-- <nickager>  ?
-- <Iceland_jack>  > let (.:) = fmap.fmap in (negate .: (*)) 4 3
-- <lambdabot> -12
-- <Iceland_jack>  (*) is a binary function
-- <Iceland_jack>  When you write
-- <Iceland_jack>  (negate . (*)) a
-- <Iceland_jack>  it's the same as
-- <Iceland_jack>  negate ((*) a)
-- <Iceland_jack>  negate (a *)
-- <Iceland_jack>  and you can't 'negate' a function (a *)
-- <Iceland_jack>  That's why you get a 'Num (a -> a)' constraint
-- <Iceland_jack>  The only step I performed was using the definition of '(.)'
-- <Iceland_jack>  @src (.)
-- <lambdabot> (f . g) x = f (g x)

-- see also:
-- "Difference between function composition operator (.) and fmap (<$>)" http://stackoverflow.com/questions/27883414/difference-between-function-composition-operator-and-fmap

-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- the text entitled - "Here’s something you can do with Applicatives that you can’t do with Functors. How do you apply a function that takes two arguments to two wrapped values?"

-- > (*) <$> Just 5 <*> Just 3
-- Just 15

-- > liftA2 (*) (Just 5) (Just 3)
-- Just 15
--
-- see http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- Functors are things that can be mapped over, like lists, Maybes, trees, and such
-- ...
-- Another instance of Functor that we've been dealing with all along but didn't know was a Functor is (->) r
-- If you ever find yourself binding the result of an I/O action to a name, only to apply a function to that and call that something else, consider using fmap, because it looks prettier.
--
-- for instance Functor ((->) r)
-- The function being mapped over a computation results in the same computation but the result of that computation is modified with the function.
--
-- if we write fmap :: (a -> b) -> (f a -> f b), we can think of fmap not as a function that takes one function and a functor and returns a functor, but as a function that takes a function and returns a new function that's just like the old one, only it takes a functor as a parameter and returns a functor as the result. It takes an a -> b function and returns a function f a -> f b. This is called lifting a function.
-- When we say a functor over numbers, you can think of that as a functor that has numbers in it. The former is a bit fancier and more technically correct, but the latter is usually easier to get.
-- You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor, or you can think of it as a function that takes a function and lifts that function so that it operates on functors. Both views are correct and in Haskell, equivalent.

-- I had a moment when I vague understood what was going on. Have a look below:
$ gchi
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> let h = readFile "confusing fmap.hs"
Prelude> :t lines
lines :: String -> [String]
Prelude> let f = lines h

<interactive>:4:15:
    Couldn't match type ‘IO String’ with ‘[Char]’
    Expected type: String
      Actual type: IO String
    In the first argument of ‘lines’, namely ‘h’
    In the expression: lines h
Prelude> fmap lines h

-- I used fmap to "lift" lines into the IO monad, so it could operate on the value inside the box.
-- wow!
