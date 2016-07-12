module TypeDirected where

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the part on "Type-directed Programming".
--
-- It contains some definitions from the slides, and a few exercises. It
-- also contains a few of the solutions to the List exercises before.

import Prelude hiding (length, elem, take, (++),
                       reverse, filter, even, odd, map,
                       fst, snd, zip, lookup)
import Data.Char

-- The first part is all about the structure of function definitions,
-- and how to systematically define functions by following the structure
-- of the datatypes.

--------------------------------------------------------------------------
-- Declaring new functions and constants:

length :: [a] -> Int
length []       = 0
length (x : xs) = 1 + length xs

-- Try in GHCi:
--
-- [1..10]
-- length [1..10]
-- length []
-- :t length

--------------------------------------------------------------------------
-- Another look at elem:

-- elem :: Eq a => a -> [a] -> Bool
elem x []       = False
elem x (y : ys) = x == y || elem x ys

-- The type signature is deliberately commented out.
--
-- Try in GHCi:
--
-- :t elem
--
-- See that GHC can infer the type correctly.
--
-- Try to put an explicit type signature in, but a wrong one:
--
-- elem :: a -> [a] -> Bool
--
-- Reload, look at the error message and try to understand it.
--
-- Next, try with one that's too special:
--
-- elem :: Int -> [Int] -> Bool
--
-- Then reload and try to
--
-- :t elem
--
-- again. Note that a more restrictive type signature really
-- restricts the type. Observe that
--
-- elem True [False, True]
--
-- works with the more general, but not the restricted type signature.

--------------------------------------------------------------------------
-- Another example:

take :: Int -> [a] -> [a]
take n []        =  []
take 0 (x : xs)  =  []
take n (x : xs)  =  x : take (n - 1) xs

-- Try in GHCi:
--
-- :t take
-- :t take 3
-- :t take 3 []
-- take 3 []
-- take 3 "hello"
-- take 3 [1,2]
-- take 3 [1..10]
-- take 0 [1..10]
-- take (-1) [1..10]
--
-- How could the result in the final case be fixed?

-- The function '++' is supposed to append two lists. Don't let it
-- confuse you that we get two lists to work with here. Just perform
-- the standard technique by pattern matching on the
-- first argument, leaving the second argument alone.

(++) :: [a] -> [a] -> [a]
xs ++ ys = error "TODO: implement (++)"

-- Using '++', we can define 'reverse' by following our standard
-- pattern for defining functions on lists:

reverse :: [a] -> [a]
reverse = error "TODO: implement reverse"

-- Try the functions on a number of examples.
--
-- How efficient do you think 'reverse' is?

--------------------------------------------------------------------------
-- Excursion: infix operators

-- Try in GHCi:
--
-- True && False
-- True || False
-- 2 + 3 * 4
-- 3 - 2 - 1
-- 7 `mod` 3
-- 7 `div` 3
-- (+) 2 3
-- mod 7 3
-- 3 `elem` [1,2,3]
-- :i ||
-- :i &&
-- :i +
-- :i *
-- :i -
-- :i mod
-- :i div

--------------------------------------------------------------------------
-- Defining 'filter':

filter :: (a -> Bool) -> [a] -> [a]
filter p []        =  []
filter p (x : xs)
  | p x            =  x : filter p xs
  | otherwise      =      filter p xs

--------------------------------------------------------------------------
-- Using 'filter':

even :: Integral a => a -> Bool
even x = x `mod` 2 == 0

-- The 'Integral' constraint stems from the 'mod' function -- later more.

odd :: Integral a => a -> Bool
odd x = x `mod` 2 == 1

-- Redefine 'odd' to instead be the negation of 'even'.

positiveInt :: Int -> Bool
positiveInt n = n > 0

palindrome :: String -> Bool
palindrome xs = reverse xs == xs

-- Try in GHCi:
--
-- palindrome []
-- palindrome "otto"
-- palindrome "hello"
-- filter palindrome ["a", "b", "ab", "ba", "abba"]
-- filter even [1..10]

-- Let's try to define all strings made up of 'a's and 'b's:

abStrings :: [String]
abStrings = [] : [ new | xs <- abStrings, new <- ['a' : xs, 'b' : xs] ]

-- You don't have to understand how exactly it works for now. Note
-- though that it's defined recursively and produces an infinite list.
--
-- Try in GHCi (stop with Ctrl-C):
--
-- abStrings
--
-- If you take a finite amount of elements, it stops:
--
-- take 100 abStrings
--
-- Now try to get at the first 100 palindromes in the list:
--
-- take 100 (filter palindrome abStrings)
--
-- Will the following expressions terminate? What's the result?
-- Predict first, then try:
--
-- "abba" `elem` filter palindrome abStrings
-- "aaba" `elem` filter palindrome abStrings
-- "aaba" `elem` take 100 (filter palindrome abStrings)

--------------------------------------------------------------------------
-- Excursion: anonymous functions:

-- Try in GHCi:
--
-- filter (\ n -> n > 10 && even n) [1..100]
-- :t \ n -> n > 10 && even n

myPredicate = \ n -> n > 10 && even n

--------------------------------------------------------------------------
-- Excursion: operator sections:
--
-- Try in GHCi:
--
-- :t (>10)
-- :t (10>)
-- (>10) 12
-- (10>) 12
-- (\ x -> x > 10) 12
-- filter (>10) [1..100]

--------------------------------------------------------------------------
-- Exercise: definining 'map'
--
-- Just use the standard design principle (and don't look back to
-- the QuickTour file, because that contains the solution already):

map :: (a -> b) -> [a] -> [b]
map = error "TODO: implement map"

-- Try in GHCi:
--
-- :t map
-- :t map (>10)
-- :t map even [1..10]
-- :t map (+1) [1..10]
-- :t map (*2) [1..10]
-- :t map (\ x -> show x ++ show x) [1..10]

--------------------------------------------------------------------------
-- Example tuples:

example :: (Bool, String)
example = (True, "yes, it's true")

triple :: ([a] -> [a], [b] -> Int, Char)
triple = (reverse, length, 'x')

-- Try in GHCi:
--
-- :t ((++), (*), (+), not)
-- :t ("hello", "world")

--------------------------------------------------------------------------
-- Example: selecting the first component of a pair:

fst :: (a, b) -> a
fst (x, y) = x

-- Try to define 'snd' correspondingly.
--
-- Try in GHCi:
--
-- fst example
-- fst triple
--
-- Note that each tuple type is a new type, and incompatible with the
-- others.

--------------------------------------------------------------------------
-- Zipping two lists:

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _        _        = []

-- Try a few things with zip:
--
-- zip [1..10] ['a'..]
-- zip [1..10] (map even [1..10])

--------------------------------------------------------------------------
-- Association lists:

numbers :: [(Int, String)]
numbers = [(1, "one"), (5, "five"), (42, "forty-two")]

--------------------------------------------------------------------------
-- Defining 'lookup' -- bad version:

lookupBad :: Eq key => key -> [(key, val)] -> val
lookupBad x []            = error "lookup: unknown key"
lookupBad x ((k, v) : ys)
  | x == k                = v 
  | otherwise             = lookupBad x ys

-- Try in GHCi:
--
-- lookupBad 5 numbers
-- lookupBad 4 numbers

--------------------------------------------------------------------------
-- The 'Maybe' datatype:

-- Try in GHCi:
--
-- :t Nothing
-- :t Just

--------------------------------------------------------------------------
-- Defining 'lookup' -- good version:

lookup :: Eq key => key -> [(key, val)] -> Maybe val
lookup x []            = Nothing
lookup x ((k, v) : ys)
  | x == k             = Just v 
  | otherwise          = lookup x ys

-- Try in GHCi:
--
-- lookup 5 numbers
-- lookup 4 numbers

--------------------------------------------------------------------------
-- Handling exceptions with 'Maybe':

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing  = def
fromMaybe def (Just x) = x

-- Try in GHCi:
--
-- fromMaybe "not found" (lookup 5 numbers)
-- fromMaybe "not found" (lookup 4 numbers)

--------------------------------------------------------------------------
-- Combining 'Maybe' computations:

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
Just x  <|> y = Just x

-- Try in GHCi:
--
-- lookup 5 numbers <|> lookup 4 numbers
-- lookup 4 numbers <|> lookup 42 numbers
-- lookup 2 numbers <|> lookup 3 numbers

--------------------------------------------------------------------------
-- New datatypes with the 'data' construct:

data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
  deriving Show -- so that we can print result values

-- Try in GHCi:
--
-- :t Mo
-- :t Fr

data Date = D Int Int Int

-- Try in GHCi:
--
-- :t D

--------------------------------------------------------------------------
-- Data constructors:

-- Try in GHCi:
--
-- :t []
-- :t (:)
-- :t (,)
-- :t (,,)
-- (,) 2 3
-- (2,3)

--------------------------------------------------------------------------
-- Booleans:

-- In deviation from the slides, we define our own copy here, because
-- if we'd redefine 'Bool' and hide the predefined one, none of the
-- functions normally available work for our new type unless we all
-- redefine them.

data MyBool = MyFalse | MyTrue
  deriving Show -- so that we can print result values

myNot :: Bool -> Bool
myNot False = True
myNot True  = False

(&&&) :: Bool -> Bool -> Bool
(&&&) True True = True
(&&&) _    _    = False

--------------------------------------------------------------------------
-- Tuples:

data Pair   a b   = MakePair a b     deriving Show
data Triple a b c = MakeTriple a b c deriving Show

-- Note that it is allowed to name a constructor and a datatype the
-- same. The namespaces are completely separate:

data Quadruple a b c d = Quadruple a b c d deriving Show

secondOfThree :: (a, b, c) -> b
secondOfThree (x, y, z) = y

secondOfThree' :: Triple a b c -> b
secondOfThree' (MakeTriple x y z) = y

--------------------------------------------------------------------------
-- Lists:

data List a = Nil a | Cons a (List a) deriving Show

--------------------------------------------------------------------------
-- Applying the design principle:

isWeekend :: Weekday -> Bool
isWeekend Sa = True
isWeekend Su = True
isWeekend _  = False

--------------------------------------------------------------------------
-- Another example:

valid :: Date -> Bool
valid (D y m d) = m >= 1 && m <= 12 && d >= 1 && d <= 31

-- Try in GHCi:
--
-- valid (D 2012 10 08)
-- valid (D 6000 40 22)
--
-- How would you improve this function?

--------------------------------------------------------------------------
-- Type synonyms with type:

type Year  = Int
type Month = Int
type Day   = Int

data Date' = D' Year Month Day

-- It's also possible to use record syntax:
--
-- Comment the two definitions of 'Date' above,
-- and uncomment the one below:

-- data Date = D { year :: Year, month :: Month, day :: Day}

-- Constructing values works normally:

exampleDate = D 2012 10 8

-- But this now also works:
--
-- exampleDate = D { year = 2012, month = 10, day = 8 }
--
-- Field names can also be used in patterns:
--
-- validMonth :: Date -> Bool
-- validMonth (D { month = m }) = m >= 1 && m <= 12
--
-- Or as functions:
--
-- validMonth :: Date -> Bool
-- validMonth d = month d >= 1 && month d <= 12
--
-- Try in GHCi:
--
-- :t month

--------------------------------------------------------------------------
-- The 'case' construct:

length' :: [a] -> Int
length' xs = case xs of
  []       -> 0
  (x : xs) -> 1 + length' xs

--------------------------------------------------------------------------
-- A better 'reverse':

reverseAcc :: [a] -> [a] -> [a]
reverseAcc  acc  []        =  acc
reverseAcc  acc  (x : xs)  =  reverseAcc (x : acc) xs

reverse' :: [a] -> [a]
reverse' = reverseAcc []

--------------------------------------------------------------------------
-- Local declarations using 'where':

reverse'' :: [a] -> [a]
reverse'' xs = go xs []
  where
    go :: [a] -> [a] -> [a]
    go []       acc = acc
    go (x : xs) acc = go xs (x : acc)

--------------------------------------------------------------------------
-- Another example using 'where':

map' :: (a -> b) -> [a] -> [b]
map' f = go
  where
    go []        =  []
    go (x : xs)  =  f x : go xs

--------------------------------------------------------------------------
-- Local declarations using 'let':

validList :: [a] -> Bool
validList xs =  let l = length xs
                in  l >= 10 && l <= 30

-- Rewrite 'validList' using 'where'.

