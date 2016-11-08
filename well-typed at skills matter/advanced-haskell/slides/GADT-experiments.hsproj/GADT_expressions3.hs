-- Existential types

{-#LANGUAGE GADTs, KindSignatures #-}

module GADT_expressions3 where

-- generalized algebraic datatype
data Expr :: * -> * where
  Int :: Int -> Expr Int
  Bool :: Bool -> Expr Bool
  IsZero :: Expr Int -> Expr Bool
  Plus :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Pair :: Expr a -> Expr b -> Expr (a, b)
  Fst :: Expr (a,b) -> Expr a
  Snd :: Expr (a,b) -> Expr b
  
eval :: Expr a -> a
eval (Int n)  = n
eval (Bool b) = b
eval (IsZero e) = eval e == 0
eval (Plus e1 e2) = eval e1 + eval e2
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3
eval (Pair x y) = (eval x, eval y)
eval (Fst p) = fst (eval p)
eval (Snd p) = snd (eval p)

-- we know there exists a type t such that p::Expr(Int, t), but we have no idea what type t is...
-- Types using this form of type hiding are called existential types.
example :: Expr Int -> Int
example (Fst p) = fst (eval p) + snd(eval p)
