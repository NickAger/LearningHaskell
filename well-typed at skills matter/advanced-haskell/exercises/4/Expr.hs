-- Expr.hs
-- Copyright (c) 2013 Well-Typed LLP

-- Start with a simple expression datatype such as this one:

type Name = String

data Expr = Num Int                    -- number literal
          | Add Expr Expr              -- addition
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2

text :: Expr -> String
text (Num n)     = show n
text (Add e1 e2) = "(" ++ text e1 ++ " + " ++ text e2 ++ ")"

-- The following exercises are about gradually extending the
-- Expr datatype and adapting the eval function. If you like,
-- you can try to adapt the text function as well -- but it
-- is not necessary if you want to save time.
--
-- (1) Make the evaluator monadic by using the Identity monad.
--
--     Turn the eval function into a pair of functions
--
--       evalM :: Expr -> Identity Int
--       eval  :: Expr -> Int
--
--     where evalM has the monadic computation, and eval runs
--     the monad.
--
-- (2) Extend with
--
--       Var Name                   -- variable
--
--     as on the slides. Use reader and error monad transformers.
--
-- (3) Now extend with
--
--       Let Name Expr Expr         -- local name binding
--
--     For the time being, assume that let-bindings are not
--     recursive.
--
-- (4) Extend with
--
--       Trace String Expr          -- debug message
--
--     Use a writer monad transformer to have the evaluator
--     accumulate a log of trace messages.
--
-- (5) Extend with
--
--       Bool Bool                  -- boolean literal
--       IfThenElse Expr Expr Expr  -- conditional
--
--     What happens to the evaluator now?
--
-- (6) Extend with
--
--       Seq Expr Expr              -- sequence
--       Assign Name Expr           -- imperative assignment
--
--     Now we allows mutable variables. This will change the
--     underlying monad to have true state.
--
-- (7) Add more constructs as you like: exceptions, IO,
--     function calls, more types, ...
