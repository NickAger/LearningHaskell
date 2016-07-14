module Monoid7 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit
import MonoidLaws
import Text.Show.Functions

newtype Comp a = Comp { unComp :: (a -> a) } deriving Show

instance Semigroup (Comp a) where
    f1 <> f2 = Comp $ (unComp f1) . (unComp f2)
    
instance Monoid (Comp a)  where
  mempty = Comp id
  mappend = (<>)
    
-- see SemiGroup9 for more details
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary
   
f = Comp $ \n -> (n + 1)
g = Comp $ \n -> (n - 1)

test1 = TestCase (assertEqual ""  (unComp (f <> g) $ 0) 0)
test2 = TestCase (assertEqual "" (unComp (f <> g) $ 1) 1)
test3 = TestCase (assertEqual "" (unComp (f <> f) $ 1) 3)
test4 = TestCase (assertEqual "" (unComp (g <> f) $ 1) 1)

test5 = TestCase (assertEqual "" (unComp (mappend f mempty) $ 1) (Sum 2))
   
-- can't use semigroupAssoc as Eq isn't defined for functions...
-- instead use QuickCheck to generate a random value which is applied
-- to both combined functions producing a result which can be tested 
-- for equality
compAssoc :: (Eq a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = (unComp (a <> (b <> c)) $ v) == (unComp ((a <> b) <> c)  $ v)

type CompAssoc = Int -> Comp Int -> Comp Int -> Comp Int -> Bool

compLeftIdentity :: Eq a => a -> Comp a -> Bool
compLeftIdentity x a = (unComp (mempty <> a) $ x) == (unComp a $ x)

compRightIdentity :: Eq a => a -> Comp a -> Bool
compRightIdentity x a = (unComp (a <> mempty) $ x) == (unComp a $ x)

main :: IO ()
main =  do
  quickCheck (compAssoc :: CompAssoc)
  quickCheck (compLeftIdentity :: Int -> Comp Int -> Bool)
  quickCheck (compRightIdentity :: Int -> Comp Int -> Bool)
  
  counts <- runTestTT (TestList [test1, test2, test3, test4, test5])
  print counts
  return ()