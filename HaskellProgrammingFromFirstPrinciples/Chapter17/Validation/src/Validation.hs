module Validation where

import Data.Either.Validation
import Control.Applicative
import Data.Semigroup

data Errors =
   DividedByZero
 | StackOverflow
 | MooglesChewedWires
 deriving (Eq, Show)

-- using Validation rather than Either ...
success :: Validation [Errors] Int
success = Success (+1) <*> Success 1

failure = Success (+1) <*> Failure [StackOverflow]

failure' :: Validation [Errors] Int
failure' = Failure [StackOverflow] <*> Success (+1)

failures :: Validation [Errors] Int
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
