-- Welcome.
--
-- This file accompanies the Quick Tour.
-- You can work through it during the presentation, testing things
-- as we go along. All the code from the slides is contained in
-- this file. You usually just have to uncomment it, then reload
-- the file in GHCi by typing :r on the GHCi prompt.
--
-- We have to hide some definitions because they already exist.
-- We also have to import some functions for later. Other modules
-- can only be imported at the beginning of a Haskell file.

import Prelude hiding (elem, (||), (++), foldr, map)
import System.Random -- for later

------------------------------------------------------------
-- Lists:

example1 = 6 : (9 : (42 : []))
example2 = 6 : 9 : 42 : []
example3 = [6, 9, 42]

------------------------------------------------------------
-- Defining a function on lists:

elem x []       = False
elem x (y : ys) = x == y || elem x ys

------------------------------------------------------------
-- Evaluation:

-- evalTest = elem 9 [6, 9, 42]

------------------------------------------------------------
-- The definition of ``or'':

infixr 2 ||   -- sets the priority; optional, but useful
True  || y = True
False || y = y

------------------------------------------------------------
-- Testing lazy evalution of ``or'':

-- lazyOrTest1 = True  || undefined
-- lazyOrTest2 = False || undefined

------------------------------------------------------------
-- Logical negation:

-- not :: Bool -> Bool -- optional, but good practice
-- not True  = False
-- not False = True

------------------------------------------------------------
-- Currying:
--
-- Try in GHCi:
--
-- :t (||)
--
-- Note that we can use operators as normal functions by
-- putting them in parentheses.

------------------------------------------------------------
-- Partial application:

-- elem :: Int -> [Int] -> Bool

-- Type signatures can appear elsewhere in the module than
-- the function, but that's usually not recommended

-- containsZero :: [Int] -> Bool
-- containsZero = elem 0

------------------------------------------------------------
-- Overloading and polymorphism:
--
-- Comment out the type signature of elem above, then try
-- in GHCi:
--
-- :t elem

------------------------------------------------------------
-- Type classes:
--
-- Try in GHCi:
--
-- :t (==)

------------------------------------------------------------
-- Overloaded literals:
--
-- Try in GHCi:
--
-- :t 23

------------------------------------------------------------
-- Other type classes:
--
-- Just for fun, try in GHCi:
--
-- :t (<=)
-- :t show
-- :t enumFromTo
-- :t minBound

------------------------------------------------------------
-- (Parametric) polymorphism:
--
-- Try in GHCi:
--
-- :t []

------------------------------------------------------------
-- Example:

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- It's common to use short names for arguments of rather
-- generic functions. It's common to give subtle indications
-- of the type, too. List variables are often suffixed with
-- an `s'.

------------------------------------------------------------
-- Data types:

-- data MyBool = MyTrue | MyFalse

-- We have to change the names here, because Bool is already
-- defined, and we have already used functions such as (==)
-- that make use of the predefined Bool type.

-- Try in GHCi:
--
-- :t True
-- :t False
-- :t MyTrue
-- :t MyFalse

------------------------------------------------------------
-- Lists:

data List a = Nil | Cons a (List a)

-- Try in GHCi:
--
-- :t []
-- :t (:)
-- :t Nil
-- :t Cons

------------------------------------------------------------
-- Generic list traversals:

-- elem' x ys  =  foldr (\ y r -> x == y || r)  False  ys
-- xs +++ ys   =  foldr (\ x r -> x : r)        ys     xs

-- elem'Test  = elem 9 [6, 9, 42]
-- concatTest = [1, 2, 3] +++ [4, 5, 6]

-- Just for fun, we include the definition of foldr:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op e []       = e
foldr op e (x : xs) = op x (foldr op e xs)

------------------------------------------------------------
-- Anonymous functions:

-- These two are equivalent:

-- double1 = \ x -> x * 2
-- double2 x = x * 2

-- Try in GHCi:
--
-- :t double1
-- :t double2
-- :t \ x -> x * 2
-- :t \ x -> x + x
-- :t \ y r -> x == y || r
-- :t \ x r -> x : r

-- These are also all equivalent:

-- cons1 = \ x r -> x : r
-- cons2 = \ x -> \ r -> x : r
-- cons3 x r = x : r
-- cons4 x r = (:) x r  -- operator as prefix function
-- cons5 = (:)          -- no need to first abstract and then apply

------------------------------------------------------------
-- Applying a function to all list elements:

-- map :: (a -> b) -> [a] -> [b]
-- map f []        =  []
-- map f (x : xs)  =  f x : map f xs

-- inc = map (\ x -> x + 1)
-- dbl = map (\ x -> x * 2)

-- Test in GHCi:
--
-- :t inc
-- :t dbl
-- :t even
-- inc [1,2,3]
-- dbl [1,2,3]
-- map even [1,2,3]

------------------------------------------------------------
-- Explicit effects:

-- Try in GHCi:
--
-- :t randomRIO
-- :t getLine
--
-- Functions with an IO-tagged result type can be executed
-- directly in GHCi. Try to run the following examples:

-- ioExample1 :: IO String
-- ioExample1 = getLine -- reads a line from screen and returns it

-- ioExample2 :: IO Int
-- ioExample2 = randomRIO (1,10) -- generates a random number between 1 and 10

-- Try running ioExample2 multiple times. Note that different
-- results are returned. This can *only* happen for IO-tagged
-- computations.

------------------------------------------------------------
-- Sequencing effectful computations:
--
-- Try in GHCi:
--
-- putStrLn getLine
--
-- This will yield a type error, stating that you're trying
-- to use an IO String in a position where a String is
-- expected.

------------------------------------------------------------
-- Echoing a string:

-- echo :: IO ()
-- echo = getLine >>= putStrLn

-- Try in GHCi:
--
-- echo
--
-- This will wait for you to enter a line, and the print it
-- again. Note that the computation prints the line as an
-- effect, but returns the rather uninformative value () that
-- is pronounced "unit".

------------------------------------------------------------
-- Syntax for sequencing

-- echo2 :: IO ()
-- echo2 = do str <- getLine
--            putStrLn str

-- This will behave exactly the same as the previous example.

------------------------------------------------------------
-- The main program:

main :: IO ()
main = putStrLn "Hello world!"

-- Try to compile this file and execute the resulting binary
-- from the command line or your desktop.
