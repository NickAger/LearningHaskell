-- Vec.hs
-- Copyright (c) 2013 Well-Typed LLP
{-# LANGUAGE GADTs, KindSignatures #-}

data Zero
data Suc n

data SNat :: * -> * where
  SZero :: SNat Zero
  SSuc  :: SNat n -> SNat (Suc n)

data Vec :: * -> * -> * where
  Nil  :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Suc n)

-- (1) Write a number of the functions we have seen.
--
-- (2) Can you define a "replicate" function for vectors?
--
-- (3) Can you define a "safe" lookup function for vectors?
--     This is tricky. You will need a GADT of "pointers" for
--     a vector of particular length. Ask for help if you
--     want to do this.
--
-- (4) Think of more functions or variations of vectors to
--     define.
--
-- (5) Can you use type families to write a type-safe function
--     in the style of C's (or Haskell's) printf?
--
-- (6) Make your expression evaluator GADT-based.
