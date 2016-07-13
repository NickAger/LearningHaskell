-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
-- http://brianshourd.com/posts/2013-01-15-tilt-foldable.html
-- https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Foldable.html

--instance Num a => Monoid (Sum a) where
--        mempty = Sum 0
--        mappend = coerce ((+) :: a -> a -> a)
----        Sum x `mappend` Sum y = Sum (x + y)

-- | Monoid under addition.
--newtype Sum a = Sum { getSum :: a }
--        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

---- | Boolean monoid under conjunction ('&&').
--newtype All = All { getAll :: Bool }
--        deriving (Eq, Ord, Read, Show, Bounded, Generic)
--
--instance Monoid All where
--        mempty = All True
--        All x `mappend` All y = All (x && y)
--
---- | Boolean monoid under disjunction ('||').
--newtype Any = Any { getAny :: Bool }
--        deriving (Eq, Ord, Read, Show, Bounded, Generic)
--
--instance Monoid Any where
--        mempty = Any False
--        Any x `mappend` Any y = Any (x || y)

import Control.Monad.Writer
import Control.Monad.State

fact0 :: Integer -> Integer
fact0 0 = 1
fact0 n = n * fact0 (n - 1)

fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
   let n' = n - 1
   tell $ "We've taken one away from " ++ show n ++ "\n"
   m <- fact1 n'
   tell $ "We've called f " ++ show m ++ "\n"
   let r = n * m
   tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
   return r

fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
  let n' = n-1
  tell $ Sum 1
  m <- fact2 n'
  let r = n*m
  tell $ Sum 1
  return r
  

fact3 :: Integer -> State Integer Integer
fact3 0 = return 1
fact3 n = do
  let n' = n-1
  modify (+1)
  m <- fact3 n'
  let r = n*m
  modify (+1)
  return r

fact4 :: Integer -> Writer Any Integer
fact4 0 = return 1
fact4 n = do
  let n' = n-1
  m <- fact4 n'
  let r = n*m
  tell (Any (r==120))
  return r

-- accumulate in opposite order to fact1
fact5 :: Integer -> Writer (Dual String) Integer
fact5 0 = return 1
fact5 n = do
  let n' = n-1
  tell $ Dual $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact5 n'
  tell $ Dual $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tell $ Dual $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r
  
tellFst a = tell $ (a,mempty)
tellSnd b = tell $ (mempty,b)

-- the moniod is a tuple (String, Sum Integer)
fact6 :: Integer -> Writer (String,Sum Integer) Integer
fact6 0 = return 1
fact6 n = do
  let n' = n-1
  tellSnd (Sum 1)
  tellFst $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact6 n'
  let r = n*m
  tellSnd (Sum 1)
  tellFst $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r
