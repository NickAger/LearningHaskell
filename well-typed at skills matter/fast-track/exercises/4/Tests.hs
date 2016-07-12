module Main where

import BasicFunctions
import Guess
import Test.QuickCheck

--------------------------------------------------------------------------
-- Introduction:
-- 
-- In this module, we'll try to write tests for several other pieces of
-- code we have already worked with.

-- If you have implemented a different sorting algorithm than the
-- insertion sort we've just discussed, then test your sorting
-- algorithm.

-- In BasicFunctions, we have defined encoding and decoding for run-length
-- encoding. Test whether encoding and then decoding an arbitrary String
-- yields the original result.
--
-- What about the other direction?

-- If you have implemented the spell checker, then compile it with HPC
-- enabled. Run it on a number of example documents and dictionaries,
-- then check for coverage. Do you reach full coverage?

--------------------------------------------------------------------------
-- Testing the number-guessing game:
--
-- This assumes that you have implemented a solver for the guess game.
--
-- One way to gain some trust in the solver is to see if it
-- terminates and produces the right numbers in a larger number of
-- situations.

playRange :: [Int]
playRange = map (\ n -> play (pick n) guess (Q (1, 100) 50)) [1..100]

-- Try in GHCi:
--
-- [1..100]
-- playRange
--
-- Note that the first is a simple way to generate a list of
-- subsequent numbers. The second call should produce the *same*
-- list if everything is fine. In other words, if you call
-- 'testPlay' as defined here

testPlay :: Bool
testPlay = playRange == [1..100]

-- it should evaluate to 'True'.

--------------------------------------------------------------------------
-- More testing:

-- There are a couple of implicit invariants in our implementation
-- of the number guessing game. For example, if we look at a question,
-- then it contains a range and a guess. For the range, we certainly
-- assume that the upper bound is at least the lower bound.
-- Furthermore, it seems reasonable to require that the guess is
-- actually *within* the range.
--
-- Write a function:

validQuestion :: Question -> Bool
validQuestion = error "TODO: implement validQuestion"

-- that captures these requirements. Note that you can use (<=)
-- rather than 'compare' as a comparison operator returning a 'Bool',
-- and that you can use (&&) to combine requirements.

-- We consider a 'GameState' to be valid if it is a 'Done'-state or if
-- it contains a valid question. Implement this in the following
-- function:

validGameState :: GameState -> Bool
validGameState = error "TODO: implement validGameState"

-- Given these two functions, we can now state a testable property:

guessPreservesValidity :: Question -> Answer -> Property
guessPreservesValidity q a = validQuestion q ==> validGameState (guess q a)

-- The property says that if we start if a valid question 'q' and
-- any answer 'a', then 'guess' should move to a valid state.

-- In order for auto-generation to work, we have to explain
-- how to come up with reasonable random values of type 'Question'
-- and 'Answer' (synyonmous to 'Ordering') by providing instances
-- of class 'Arbitrary'.
--
-- Define an instance for Ordering:
--
-- instance Arbitrary Ordering where ...
--
-- Note that Ordering is an enumeration type.
--
-- Then define an instance for Question:
--
-- instance Arbitrary Question where ...
--
-- Recall that Question is defined as
--
-- data Question = Q Range Guess deriving Show
-- type Range    = (Int, Int)
-- type Guess    = Int
--
-- So in essence, a question conists of three integers. QuickCheck knows
-- how to generate triple of integers. Use fmap to change such a triple
-- into a question.

--------------------------------------------------------------------------
-- More tasks:

-- * Now that we know that the property doesn't hold, we would of
-- course like to fix the program. Come up with a modified design for
-- which the property (or a slightly modified one) would hold.
--
-- * Think about additional properties about our game that we could
-- test, and test them.
