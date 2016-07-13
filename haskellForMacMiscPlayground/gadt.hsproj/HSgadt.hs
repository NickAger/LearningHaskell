-- https://en.wikibooks.org/wiki/Haskell/GADT

data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test

eval :: Expr -> Maybe (Either Int Bool)
-- Your implementation here.