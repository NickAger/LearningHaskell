import Control.Monad

-- 32.1.1 The `guard` function
evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value
  

{-
from "32.1.1 The `guard` function":
To understand `guard`, it helps tremendously to translate
from `do`-notation back to `>>=`, `>>`, and lambdas. Learning
about `guard` will also teach you a lot about the subtleties of
`>>`.
-}

{-
 do notation de-sugar syntax at: http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html
-} 

evensGuard2 :: Int -> [Int]
evensGuard2 n = 
  [1 .. n] >>= (\value -> (guard (even value)) >> [value]) 
  
-- but how does this work???
-- see opposite page
