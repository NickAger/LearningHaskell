module ArbitrarySum where
  
import Test.QuickCheck
import Data.Monoid

-- the module designed to avoid the warning:
--  "No instance for (Arbitrary (Sum Int))"
  
-- see http://austinrochford.com/posts/2014-05-27-quickcheck-laws.html
instance (Arbitrary a) => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary
    
-- my attempts to understand the above:
--instance (Arbitrary a) => Arbitrary (Sum a) where
--    arbitrary = fmap Sum arbitrary

--instance (Arbitrary a) => Arbitrary (Sum a) where
--    arbitrary = do
--      x <- arbitrary
--      return (Sum x)

