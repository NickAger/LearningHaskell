-- from: http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

{-# LANGUAGE DataKinds #-} -- promote data types into kinds
{-# LANGUAGE KindSignatures #-} -- compile something with a kind signature eg "data Vector (n :: Nat) (a :: *)"
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-} -- "functions that operate on types are called type families"
{-# LANGUAGE UndecidableInstances #-} -- GHC, it’s okay "The type family application ‘Add n ('Succ m)’  is no smaller than the instance head (Use UndecidableInstances to permit this)"

data Nat = Zero | Succ Nat

data Vector (n :: Nat) (a :: *) where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a
  
-- the below is equivalent to the above
--data Vector n a where
--    VNil :: Vector Zero a
--    VCons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"


add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ n) m = add n (Succ m)

-- promoting add to type level

type family Add x y where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)
  

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = VNil
append (VCons a rest) xs = VCons a (append rest xs)

main = putStrLn "hello world"