module SemiGroup10 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit
import Text.Show.Functions

newtype Comp a = Comp { unComp :: (a -> a) } deriving Show


instance Semigroup (Comp a) where
    f1 <> f2 = Comp $ (unComp f1) . (unComp f2)
    
-- see SemiGroup9 for more details
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary
   
f = Comp $ \n -> (n + 1)
g = Comp $ \n -> (n - 1)

test1 = TestCase (assertEqual ""  (unComp (f <> g) $ 0) 0)
test2 = TestCase (assertEqual "" (unComp (f <> g) $ 1) 1)
test3 = TestCase (assertEqual "" (unComp (f <> f) $ 1) 3)
test4 = TestCase (assertEqual "" (unComp (g <> f) $ 1) 1)
   
-- can't use semigroupAssoc as Eq isn't defined for functions...
-- instead use QuickCheck to generate a random value which is applied
-- to both combined functions producing a result which can be tested 
-- for equality
compAssoc :: (Eq a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = (unComp (a <> (b <> c)) $ v) == (unComp ((a <> b) <> c)  $ v)

type CompAssoc = Int -> Comp Int -> Comp Int -> Comp Int -> Bool

main :: IO ()
main =  do
  quickCheck (compAssoc :: CompAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4])
  print counts
  return ()