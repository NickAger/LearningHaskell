-- from: http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

{-# LANGUAGE DataKinds #-} -- promote data types into kinds
{-# LANGUAGE KindSignatures #-} -- compile something with a kind signature
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-} -- functions that operate on types
{-# LANGUAGE UndecidableInstances #-} -- GHC, it’s okay

data Nat = Zero | Succ Nat

data Vector (n :: Nat) (a :: *) where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a
  

type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = Add n ('Succ m)
  
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ n) m = add n (Succ m)