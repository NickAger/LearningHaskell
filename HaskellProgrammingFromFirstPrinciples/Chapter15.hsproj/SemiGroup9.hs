module SemiGroup9 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit
import ArbitrarySum
import Text.Show.Functions

newtype Combine a b = Combine { unCombine :: (a -> b) } deriving Show

instance Semigroup b => Semigroup (Combine a b) where
    f1 <> f2 = Combine $ \n -> ((unCombine f1) n) <> ((unCombine f2 n))

-- arrived after an IRQ dicussion:
{-
Cale: There should already be an instance  (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
Cale: So, you can just use that,  arbitrary = do f <- arbitrary; return (Combine f)
Cale: Or simply  arbitrary = fmap Combine arbitrary

See also: http://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions
-}
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

test1 = TestCase (assertEqual "" (unCombine (f <> g) $ 0) (Sum 0))
test2 = TestCase (assertEqual "" (unCombine (f <> g) $ 1) (Sum 2))
test3 = TestCase (assertEqual "" (unCombine (f <> f) $ 1) (Sum 4))
test4 = TestCase (assertEqual "" (unCombine (g <> f) $ 1) (Sum 2))
   
-- can't use semigroupAssoc as Eq isn't defined for functions...
-- instead use QuickCheck to generate a random value which is applied
-- to both combined functions producing a result which can be tested 
-- for equality
combineAssoc :: (Semigroup b, Eq b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc v a b c = (unCombine (a <> (b <> c)) $ v) == (unCombine ((a <> b) <> c)  $ v)

type CombineAssoc = Int -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

main :: IO ()
main =  do
  quickCheck (combineAssoc :: CombineAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4])
  print counts
  return ()