module Guess where

--------------------------------------------------------------------------
-- Introduction:
--
-- In this module, we implement a simple number-guessing game. It is
-- played by two parties, called an "Oracle" and a "Solver". The
-- Oracle should pick a number and reply to questions the Solver asks.
--
-- The purpose of this development is to show how a deeply interactive
-- process can without problems be modelled in a purely functional way.
--
-- We will run it using IO in the end, but note how little of the code
-- is actually affected by that.

--------------------------------------------------------------------------
-- Interface:

-- Next, let's define lots of types. Types are a vital ingredient of
-- Haskell programs and are very easy to define.

-- The Solver makes guesses. Guesses are just numbers.

type Guess = Int

-- The Oracle has to deal with guesses and provide answers. For the
-- purpose of this game, we assume the implicit question to always be
-- whether the number the Oracle picked was less than, equal, or
-- greater than the number the Solver guessed. We therefore introduce
-- another type synonym:

type Answer = Ordering

-- Recall that 'Odering' is defined as follows:
--
-- data Ordering = LT | EQ | GT
--
-- We can now capture that the Oracle is supposed to turn guesses into
-- answers:

type Oracle = Guess -> Answer

-- Let's think about the type of the Solver. A first approximation could
-- be
--
-- type Solver = Answer -> Guess
--
-- but there are two problems with this type:
--
--   * Since the Answer is just LT, EQ, or GT, it's actually not enough
--     information to produce a new Guess; we need to keep track of some
--     data, namely the range in which we expect the number to be and
--     the guess we last made.
--
--   * We should not always return a new Guess. What if the answer was
--     EQ, or if the information we have collected is enough to conclude
--     the right number without doubt? Then we're done! So we need a
--     choice for the result type.
--
-- In order to address the first problem, we introduce the Question type:

data Question = Q Range Guess deriving Show
type Range    = (Int, Int)

-- A Question pairs a range with a guess, so we keep track of the
-- information about the interval we expect our number in. A range is
-- a pair of a lower bound and an upper bound, where we understand bounds
-- to be inclusive.
--
-- To address the second problem with the type of 'Solver' above,
-- we'll introduce a type 'GameState' that captures the two options:

data GameState = Done Int | Continue Question deriving Show

-- We're either 'Done', then we should know the final number.
-- Or we 'Continue' with a new Question.

type Solver = Question -> Answer -> GameState

-- That's it for the interface.


--------------------------------------------------------------------------
-- Implementation:

-- Let's now implement the interface. For the 'Oracle', we just
-- implement one that memorizes a given integer number and
-- answers truthfully by comparing the given guesses with the
-- memorized number.

pick :: Int -> Oracle
pick s g = compare s g

-- Recall that 'Oracle' is a function type, so 'pick' really
-- takes two arguments, even though it looks from the type
-- as if it takes only one.

-- Next, we implement a really dumb solver. One that doesn't
-- actually make any attempt at coming to a solution. If it
-- by accident picks the right number, it'll stop asking.
-- Otherwise, it will just stick to its previous guess:

dumb :: Solver
dumb (Q d g) EQ = Done g
dumb (Q d g) _  = Continue (Q d g)

-- Note how we always use pattern matching to split the problem
-- and destruct the input data.

--------------------------------------------------------------------------
-- Playing the game:

-- How do we plug an Oracle and a Solver together? We implement a
-- function 'play' for this. It takes an Oracle and a Solver, and an
-- initial Question. It repeatedly lets the solver and oracle interact
-- (by recursion), until we reach 'Done'.

play :: Oracle -> Solver ->
        Question -> Int
play oracle solver = go
  where go (Q d g) = case solver (Q d g) (oracle g) of
                       Done     n  -> n
                       Continue q -> go q

-- In order to use 'play', we need an initial question:

initialQuestion :: Question
initialQuestion = Q (1, 100) 42

-- Now we can try our simple oracle with the dumb solver.

playDumb :: Int -> Int
playDumb n = play (pick n) dumb initialQuestion

-- Try in GHCi:
--
-- playDumb 42
-- playDumb 50
-- playDumb 99
--
-- Explain the results.

-- Here is a variation of 'play' that does not just
-- produce the number once it has been guessed, but also
-- all the questions that have been asked.

type Trace = ([Question], Int)

playTrace :: Oracle -> Solver ->
             Question -> Trace
playTrace oracle solver = go
  where go (Q d g) = case solver (Q d g) (oracle g) of
                       Done     n -> ([], n)
                       Continue q -> let (qs, r) = go q in (q : qs, r)

playTraceDumb :: Int -> Trace
playTraceDumb n = playTrace (pick n) dumb initialQuestion

-- Try in GHCi:
--
-- playTraceDumb 42
-- playTraceDumb 50
--
-- See lazy evaluation in action. Explain the results.

-- Finally, we will write another variation of 'play'
-- that let's you play interactively as the oracle. For
-- this, we'll have to change the type of 'Oracle' to
-- allow interaction with the outside world:

type InteractiveOracle = Guess -> IO Answer

-- The function 'playInteractively' takes an 'InteractiveOracle'
-- rather than an 'Oracle'. It also involves interaction, therfore
-- it itself gets an IO-tagged result type.

playInteractively :: InteractiveOracle -> Solver ->
                     Question -> IO Int
playInteractively oracle solver = go
  where go (Q d g) = do
                       a <- oracle g
                       case solver (Q d g) a of
                         Done     n  -> return n
                         Continue q' -> go q'

-- Note that apart from the sequencing and the type change, the
-- structure is still very similar to that of 'play'.

-- We now implement a very simple interactive oracle that lets you
-- play:

user :: InteractiveOracle
user n = do
           putStrLn $ "Is " ++ show n ++ " your number? (press 'l', 'e' or 'g')"
           c <- getChar
           putStrLn ""
           case c of
             'l' -> return LT
             'e' -> return EQ
             'g' -> return GT
             _   -> do
                      putStrLn "what? try again"
                      user n

playInteractivelyDumb :: IO Int
playInteractivelyDumb = playInteractively user dumb initialQuestion

-- Try in GHCi:
--
-- playInteractivelyDumbo

--------------------------------------------------------------------------
-- What next?

-- If you like, you can move to the GuessWeb module now and see how we
-- can provide a simple web-based interface for our guessing game.
--
-- In the rest of this module, we'll explore how to actually write a
-- proper solver and how to test it.


--------------------------------------------------------------------------
-- A better solver:

-- The proper way of solving is of course to try a binary division of
-- the search space in each step. So the goal is to define a function

guess :: Solver
guess (Q d g) EQ = Done g
guess (Q (l,u) g) answer  = case answer of
  LT -> Continue (Q ((middle (g, u)), u) (middle (l, g)))
  GT -> Continue (Q (l, (middle (l, g)))  (middle (g, u)))

playInteractivelyGuess :: IO Int
playInteractivelyGuess = playInteractively user guess initialQuestion                  

middle :: Range -> Guess
middle (l, u) = ((u-l) `quot` 2) + l


-- that implements this strategy. Remove the dummy definition above
-- and write your own instead. Try to apply pattern matching
-- consistently. Note that you can perform nested pattern matches, for
-- example on the pair within the question. You might want to define a
-- helper function
--
-- middle :: Range -> Guess
--
-- that, given a 'Range', calculates the approximate middle.
--
-- Once you have implemented the function and it type-checks, try to
-- plug it together with the 'pick' and 'user' oracles and test
-- 'play', 'playTrace' and 'playInteractively' again (and, if you
-- like, try the web interface as well).

--------------------------------------------------------------------------
-- More tasks:

-- * Try to modify 'playTrace' to not print the list of questions,
-- but the number of guesses.
--
-- * Try to modify 'playRange' to print for each possible memorized
-- number in the range from 1 to 100 the number of guesses it takes.
-- Note that you can use 'fst' or pattern matching to select the first
-- component of a pair.
--
-- * Try to use 'maximum' to figure out what the maximal number of
-- guesses is in 'playRange'. Parameterize the resulting chain of
-- functions such that the range becomes configurable rather than
-- being fixed to (1, 100).
