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
Applicative
-}
