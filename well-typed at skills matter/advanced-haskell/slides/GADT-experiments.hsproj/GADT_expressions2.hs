{-#LANGUAGE GADTs, KindSignatures #-}

module GADT_expressions2 where

-- generalized algebraic datatype
data Expr :: * -> * where
  Int :: Int -> Expr Int
  Bool :: Bool -> Expr Bool
  IsZero :: Expr Int -> Expr Bool
  Plus :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  
eval :: Expr a -> a
eval (Int n)  = n
eval (Bool b) = b
eval (IsZero e) = eval e == 0
eval (Plus e1 e2) = eval e1 + eval e2
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3

{-
Note the above can also be written as:

data Expr a where ...

- not sure what KindSignatures brings
-}

{- 
original version
  
data Expr a = 
    Int Int
  | Bool Bool
  | IsZero (Expr Int)  | Plus (Expr Int) (Expr Int)  | If (Expr Bool) (Expr a) (Expr a)
-}      