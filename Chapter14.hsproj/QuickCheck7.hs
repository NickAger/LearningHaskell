{-# OPTIONS_GHC -Wall #-}

module QuickCheck7 where

import Test.QuickCheck

-- Test that reversing a list twice is the same as the identity of the list:
-- reverse . reverse == id

prop_doubleReverse :: Eq a => [a] -> Bool
prop_doubleReverse xs = (reverse . reverse) xs == id xs


-- from https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#5.0
prop_rev_rev xs = reverse (reverse xs) == xs
   where types = xs::[Int]


runQc :: IO ()
runQc = do
  quickCheck prop_doubleReverse