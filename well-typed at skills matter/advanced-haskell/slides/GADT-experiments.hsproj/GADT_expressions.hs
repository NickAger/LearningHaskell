-- without GADTs
-- it's suboptimal because:
-- * Evaluation code is mixed with code for handling type errors
-- * The evaluator uses tags (i.e.,constructors) to distinguish values – these tags are maintained and checked at run time.

module GADT_expressions where
  
data Expr = 
    Int Int
  | Bool Bool
  | IsZero Expr  | Plus Expr Expr  | If Expr Expr Expr
  
data Val = 
    VInt Int
  | VBool Bool
  
eval :: Expr -> Val
eval (Int n)  = VInt n
eval (Bool b) = VBool b
eval (IsZero e) = 
  case eval e of
    VInt n  -> VBool (n == 0)
    _       -> error "type error"
eval (Plus e1 e2) = 
  case (eval e1, eval e2) of
    (VInt n1, VInt n2)  -> VInt (n1 + n2)
    _                   -> error "type error"
eval (If e1 e2 e3) = 
  case eval e1 of
    VBool b -> if b then eval e2 else eval e3
    _       -> error "type error"
    

 
        