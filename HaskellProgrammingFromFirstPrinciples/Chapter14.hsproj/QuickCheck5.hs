{-# OPTIONS_GHC -Wall #-}

module QuickCheck5 where

import Test.QuickCheck

-- We mentioned in one of the first chapters that there are some laws involving the relationship of quot and rem and div and mod. Write QuickCheck tests to prove them.
quotRemLaw x y =  (quot x y)*y + (rem x y) == x
  
quotDivMod x y =
  (div x y)*y + (mod x y) == x
  
prop_quotRemLaw :: Integral a => a -> NonZero a -> Bool
prop_quotRemLaw x (NonZero y) = quotRemLaw x y

prop_quotDivMod :: Integral a => a -> NonZero a -> Bool
prop_quotDivMod x (NonZero y) = quotDivMod x y


runQc :: IO ()
runQc = do
  quickCheck prop_quotRemLaw
  quickCheck prop_quotDivMod