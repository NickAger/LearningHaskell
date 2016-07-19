module ArbitrarySum where

import Test.QuickCheck
import Data.Monoid

instance (Arbitrary a) => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary
