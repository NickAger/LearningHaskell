{-#LANGUAGE GADTs, KindSignatures #-}

module Question1 where
  
data X :: * -> * where
  C :: Int -> X Int
  D :: X a
  
-- all of the three type signatures below are valid:
--f :: X Int -> [Int]
--f :: X a -> [Int]
f :: X a -> [a]
f (C n) = [n]
f D = []

-- Because of this, type inference for GADT pattern matches is generally not possible, and type signatures for such functions are required
