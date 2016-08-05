{-# LANGUAGE OverloadedStrings #-}

module TextFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- Exercise: Units of Success

parseIntegersEOF :: Parser Integer
parseIntegersEOF = integer >>= (\i -> eof >> return i)

{-
nickager: The exercise: Try rewriting - parseString (integer >> eof) mempty "123" - to return “Success 123” rather than “Success ()”.
nickager: My solution: integer >>= (\i -> eof >> return i)
nickager: or is there a simpler way?
merijn: "integer <* eof" works
kadoban: integer <* eof -- I guess, but that likely uses stuff you don't know about yet
merijn: :t (<*)
lambdabot: Applicative f => f a -> f b -> f a
nickager: merijn: kadoban: thanks that simpler
-}

parseIntegersEOF' :: Parser Integer
parseIntegersEOF' = integer <* eof

{- reminder about Applicatives
From Chatper 17. Applicative

I want to do something kinda like an fmap,
but my function is embedded in the functorial structure too,
not just the value I want to apply my function to

-}

{-
I also got a little confused about the ordering of operations in
Applicative vs Monad. I was under the impression that with Applicative
operations occured in parallel whereas in Monad they are sequenced.
However this is confusing what Simon Marlow talked about. Its clearer
in Chapter 18:
1. With the MaybeApplicative, each Maybe computation fails or success
independently of each other. You're just lifting functions that are also
Just or Nothing over Maybe values.
2. With the Maybe Monad, computations contributing to the final result
can choose to return Nothing based on "previous" computations.

However I'm still a little confused at how you are sure it applies
the Applicative function to both parameters - I guess that
implicit
-}

{-
Stating the obvious, as we know its a monad it must also be an
Applicative. Opps not so obvious, something can be a Monad and
not an Applicative.
-}

{-
Very interesting comment on StackOverflow:
http://stackoverflow.com/questions/38707813/parsec-applicatives-vs-monads/38719766#38719766

It might be worth paying attention to the key semantic difference
between Applicative and Monad, in order to determine when each is appropriate.
Compare types:

(<*>) :: m (s -> t) -> m s -> m t
(>>=) :: m s -> (s -> m t) -> m t
To deploy <*>, you choose two computations, one of a function, the other of an
argument, then their values are combined by application. To deploy >>=, you
choose one computation, and you explain how you will make use of its resulting
values to choose the next computation. It is the difference between "batch mode"
and "interactive" operation.

When it comes to parsing, Applicative (extended with failure and choice to give
Alternative) captures the context-free aspects of your grammar. You will need
the extra power that Monad gives you only if you need to inspect the parse tree
from part of your input in order to decide what grammar you should use for
another part of your input. E.g., you might read a format descriptor, then an
input in that format. Minimizing your usage of the extra power of monads tells
you which value-dependencies are essential.

Shifting from parsing to parallelism, this idea of using >>= only for essential
value-dependency buys you clarity about opportunities to spread load. When two
computations are combined with <*>, neither need wait for the other.
Applicative-when-you-can-but-monadic-when-you-must is the formula for speed. T
he point of ApplicativeDo is to automate the dependency analysis of code which
has been written in monadic style and thus accidentally oversequentialised.

Your question also relates to coding style, about which opinions are free to
differ. But let me tell you a story. I came to Haskell from Standard ML, where
I was used to writing programs in direct style even if they did naughty things
like throw exceptions or mutate references. What was I doing in ML? Working on
an implementation of an ultra-pure type theory (which may not be named, for
legal reasons). When working in that type theory, I couldn't write direct-style
programs which used exceptions, but I cooked up the applicative combinators as a
way of getting as close to direct style as possible.

When I moved to Haskell, I was horrified to discover the extent to which people
seemed to think that programming in pseudo-imperative do-notation was just
punishment for the slightest semantic impurity (apart, of course, from
non-termination). I adopted the applicative combinators as a style choice (and
went even closer to direct style with "idiom brackets") long before I had a
grasp of the semantic distinction, i.e., that they represented a useful
weakening of the monad interface. I just didn't (and still don't) like the way
do-notation requires fragmentation of expression structure and the gratuitous
naming of things.

That's to say, the same things that make functional code more compact and
readable than imperative code also make applicative style more compact and
readable than do-notation. I appreciate that ApplicativeDo is a great way to
make more applicative (and in some cases that means faster) programs that were
written in monadic style that you haven't the time to refactor. But otherwise,
I'd argue applicative-when-you-can-but-monadic-when-you-must is also the better
way to see what's going on.

-}

{-
See also:
http://stackoverflow.com/questions/38707813/parsec-applicatives-vs-monads/38719766#38719766

-}

-- Exercise Try
-- Warning see: (Parsec: “try a <|> b” considered harmful)
-- http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/
-- related IRC - http://ircbrowse.net/browse/haskell-beginners?events_page=25036

{-
`try` is useful for backtracking if the
parser has already consumed input.
Often can can eliminate `try` by using `<\>`
at the point of difference eg see below:
-}

parseDecimal :: Fractional a => Parser a
parseDecimal = do
  i <- decimal
  char '.'
  d <- decimal
  return $ fromIntegral i + toDecimal d

toDecimal :: (Fractional a) => Integer -> a
toDecimal d = fromIntegral d / (10 ^^ (length $ show d))

parseDecimalOrFraction :: Parser Rational
parseDecimalOrFraction = try parseFraction <|> parseDecimal

-- without `try` and returning with Either
parseDecimalOrFraction' :: Parser (Either Rational Double)
parseDecimalOrFraction' = do
  i <- decimal
  sep <- char '.' <|> char '/'
  d <- decimal
  return $ leftOrRight sep i d
  where
    leftOrRight '.' i d = Left $ i % d
    leftOrRight '/' i d = Right $ fromIntegral i + toDecimal d

-- same as above but attempting to use Applicative rather than Monad
parseDecimalOrFraction'' :: Parser (Either Rational Double)
parseDecimalOrFraction'' =
  leftOrRight <$> decimal <*> (char '.' <|> char '/') <*> decimal
  where
    leftOrRight i '.' d = Left $ i % d
    leftOrRight i '/' d = Right $ fromIntegral i + toDecimal d
