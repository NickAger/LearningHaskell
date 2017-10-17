module ANewBeginning where
  
import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . boop

------------------------------------
-- This is the Functor of functions.
------------------------------------
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Using fmap here li s the one partially applied function over the next, in a sense setting up something like this:
-- fmap boop doop x == (*2) ((+10) x)

bbop :: Integer -> Integer 
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- bbop 3
-- ((+) <$> (*2) <*> (+10)) 3
