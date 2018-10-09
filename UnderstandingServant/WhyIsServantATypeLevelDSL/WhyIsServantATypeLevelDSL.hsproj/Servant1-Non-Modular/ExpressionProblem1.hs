module ExpressionProblem1 where
  
-- from: https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html
-- "The expression problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts)"

data Expr = I Integer | Add Expr Expr

eval :: Expr -> Integer
eval (I n)     = n
eval (Add a b) = eval a + eval b

prettyPrint :: Expr -> String
prettyPrint (I n) = show n
prettyPrint (Add a b) = unwords [prettyPrint a, "+", prettyPrint b]

{-

So, given an expression type, we can easily “add new functions over the data type”, to reuse Phil’s wording. We just write a new function. However, when the time comes to “add new cases to the data type”, this approach becomes painful. A “new case” here means a new constructor for our Expr data type. Let’s say we want to support multiplications too:

data Expr = I Integer | Add Expr Expr | Mul Expr Expr

Now, we have to modify every single function that patterns matches on an Expr to handle the Mul constructor, including our eval and prettyPrint “interpreters”. For any non-trivial domain, this becomes very painful, very quickly. Fine, so what other options are there?
-}

