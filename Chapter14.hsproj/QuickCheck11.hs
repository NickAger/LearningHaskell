{-# OPTIONS_GHC -Wall #-}

module QuickCheck11 where

import Test.QuickCheck

-- Finally, this is a fun one. You may remember we had you compose read and show one time to complete a “round trip.” Well, now you can test that it works:
--
-- f x = (read (show x)) == x

prop_readShow ::(Read a, Show a, Eq a) => a -> Bool
prop_readShow x = (read (show x)) == x 


prop_readShowActual :: Double -> Bool
prop_readShowActual = prop_readShow