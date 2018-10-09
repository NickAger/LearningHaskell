module ExpressionProblem2 where
  
-- from: https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html
-- "The expression problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts)"

-- solution described here: https://userpages.uni-koblenz.de/~laemmel/TheEagle/resources/pdf/xproblem1.pdf
-- It boils down to:
-- * Turn what would be a constructor into its own little data type.
-- * Turn what would be a simple function that operates on the data type into a typeclass with a method.
-- * Write instances of those typeclasses for the data types representing the DSL’s constructs.

-- Original data type
-- data Expr = I Integer | Add Expr Expr

data I = I Integer

-- Since we don't have an 'Expr' type anymore, to use as a type for
-- the fields of Add, we just make them type parameters. Sometimes
-- 'l' and 'r' might be I, some other times they might be 'Add I I',
-- or 'Add (Add I I) I', and so on. The type reflects the recursive
-- structure.
data Add l r = Add l r

-- an "open union" to be able to describe all the
-- valid expression types.
class Expr a
instance Expr I
instance (Expr l, Expr r) => Expr (Add l r)

-- our first interpretation, evaluation
class Expr a => Eval a where
  eval :: a -> Integer
  
-- evaluating a constant amounts to returning it
instance Eval I where
  eval (I n) = n
  
-- if we know how to evaluate two things, we know how to evaluate
-- their addition
instance (Eval l, Eval r) => Eval (Add l r) where
  eval (Add a b) = eval a + eval b
  
-- our second interpretation, pretty printing
class Expr a => Pretty a where
  pretty :: a -> String
  
instance Pretty I where
  pretty (I n) = show n

instance (Pretty l, Pretty r) => Pretty (Add l r) where
  pretty (Add a b) = unwords [pretty a, "+", pretty b]
  
-- Anyway, if we now want to add support for multiplications, we can do:

data Mul l r = Mul l r
instance (Expr l, Expr r) => Expr (Mul l r)

instance (Eval l, Eval r) => Eval (Mul l r) where
  eval (Mul a b) = eval a * eval b
  
instance (Pretty l, Pretty r) => Pretty (Mul l r) where
  pretty (Mul a b) = unwords [autoParens a, "*", autoParens b]
    where autoParens a@(Add _ _) = "(" ++ pretty a ++ ")"
          autoParens           a = pretty a
