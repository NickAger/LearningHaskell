module HuttonsRazor where
  
data Expr = 
        Lit Integer 
      | Add Expr Expr

       
eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)

