-- This is a slightly simplified form of the Zwaluw library for bidirectional
-- routing of URIs by Sjoerd Visscher and Martijn van Steenbergen.
--
-- See
--
-- https://github.com/MedeaMelana/Zwaluw
--
-- for the original version.

{-# LANGUAGE TypeOperators #-}
module UrlParser where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Maybe
import Data.Monoid (Monoid(..))
import Prelude hiding (id, (.))

data UrlParser a b = UrlParser {
  prs :: String -> [(a -> b, String)],
  ser :: b -> [(String -> String, a)]
}

instance Category UrlParser where
  id = UrlParser
    (\x -> [(id, x)])
    (\x -> [(id, x)])
  ~(UrlParser pf sf) . ~(UrlParser pg sg) = UrlParser
    (compose (.) pf pg)
    (compose (.) sf sg)

compose
  :: (a -> b -> c)
  -> (i -> [(a, j)])
  -> (j -> [(b, k)])
  -> (i -> [(c, k)])
compose op mf mg s = do
  (f, s') <- mf s
  (g, s'') <- mg s'
  return (f `op` g, s'')

instance Monoid (UrlParser a b) where
  mempty = UrlParser
    (const mzero)
    (const mzero)
  ~(UrlParser pf sf) `mappend` ~(UrlParser pg sg) = UrlParser
    (\s -> pf s `mplus` pg s)
    (\s -> sf s `mplus` sg s)

infixr 8 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

infixr 9 <.>
-- | Infix operator for composition.
(<.>) :: UrlParser b c -> UrlParser a b -> UrlParser a c
(<.>) = (.)

-- | Map over UrlParsers.
xmap :: (a -> b) -> (b -> Maybe a) -> UrlParser r a -> UrlParser r b
xmap f g (UrlParser p s) = UrlParser ((fmap . fmap . first . fmap) f p) (maybe mzero s . g)

-- | Lift a constructor-destructor pair to a pure UrlParser.
pure :: (a -> b) -> (b -> Maybe a) -> UrlParser a b
pure f g = xmap f g id

-- | Routes a constant string.
lit :: String -> UrlParser r r
lit l = UrlParser
  (\ s -> let (s1, s2) = splitAt (length l) s
          in if s1 == l then return (id, s2) else mzero)
  (\ b -> return ((l ++), b))

-- | A stack datatype. Just a better looking tuple.
data a :- b = a :- b deriving (Eq, Show)
infixr 8 :-

-- | Stack destructor.
pop :: (a -> b -> r) -> (a :- b) -> r
pop f (a :- b) = f a b

-- | Get the top of the stack.
hhead :: (a :- b) -> a
hhead (a :- _) = a

-- | Get the stack with the top popped.
htail :: (a :- b) -> b
htail (_ :- b) = b

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f = pop (c . f)

-- | Convert a UrlParser to do what it does on the tail of the stack.
duck1 :: UrlParser r1 (a :- r2) -> UrlParser (h :- r1) (a :- h :- r2)
duck1 r = UrlParser
  (map (first (\f (h :- t) -> let a :- t' = f t in a :- h :- t')) . prs r)
  (\(a :- h :- t) -> map (second (h :-)) $ ser r (a :- t))


-- | Give all possible parses.
parse :: UrlParser () a -> String -> [a]
parse p s = [ a () | (a, "") <- prs p s ]

-- | Give the first parse, for UrlParsers with a parser that yields just one value.
parse1 :: UrlParser () (a :- ()) -> String -> Maybe a
parse1 p = listToMaybe . map hhead . parse p

unparse :: UrlParser () a -> a -> [String]
unparse p = map (($ "") . fst) . ser p

-- | Give the first serialization, for UrlParsers with a serializer that needs just one value.
unparse1 :: UrlParser () (a :- ()) -> a -> Maybe String
unparse1 p = listToMaybe . unparse p . (:- ())

-- | Build a UrlParser for a value given all the ways to parse and serialize it.
val :: (String -> [(a, String)]) -> (a -> [String -> String]) -> UrlParser r (a :- r)
val rs ss = UrlParser
  (map (first (:-)) . rs)
  (\(a :- r) -> map (\f -> (f, r)) (ss a))

-- | Make a UrlParser optional.
opt :: UrlParser r r -> UrlParser r r
opt = (id <>)

-- | Repeat a UrlParser zero or more times, combining the results from left to right.
manyr :: UrlParser r r -> UrlParser r r
manyr = opt . somer

-- | Repeat a UrlParser one or more times, combining the results from left to right.
somer :: UrlParser r r -> UrlParser r r
somer p = p . manyr p


readshow :: (Show a, Read a) => UrlParser r (a :- r)
readshow = val reads (return . shows)

-- | Routes any 'Int'.
int :: UrlParser r (Int :- r)
int = readshow

-- | Routes one character satisfying the given predicate.
satisfy :: (Char -> Bool) -> UrlParser r (Char :- r)
satisfy p = val
  (\s -> [ (c, cs) | c:cs <- [s], p c ])
  (\c -> [ (c :) | p c ])

-- | Routes part of a URL, i.e. a String not containing @\'\/\'@ or @\'\?\'@.
part :: UrlParser r (String :- r)
part = rList (satisfy (\c -> c /= '/' && c /= '?'))

rNil :: UrlParser r ([a] :- r)
rNil = pure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: UrlParser (a :- [a] :- r) ([a] :- r)
rCons = pure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

-- | Converts a UrlParser for a value @a@ to a UrlParser for a list of @a@.
rList :: UrlParser r (a :- r) -> UrlParser r ([a] :- r)
rList r = manyr (rCons . duck1 r) . rNil


