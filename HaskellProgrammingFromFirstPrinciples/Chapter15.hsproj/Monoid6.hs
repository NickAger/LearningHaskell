module Monoid6 where
  
import Data.Semigroup
import Test.QuickCheck
import Test.HUnit
import ArbitrarySum
import Text.Show.Functions

newtype Combine a b = Combine { unCombine :: (a -> b) } deriving Show

instance Semigroup b => Semigroup (Combine a b) where
    f1 <> f2 = Combine $ \n -> ((unCombine f1) n) <> ((unCombine f2 n))
    
instance (Semigroup b, Monoid b)=> Monoid (Combine a b)  where
  mempty = Combine $ \_ -> mempty
  mappend = (<>)

-- see SemiGroup9 for more details
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary
   
f = Combine $ \n -> Sum (n + 1)

test1 = TestCase (assertEqual ""  (unCombine (mappend f mempty) $ 1) (Sum 2))
   
-- can't use semigroupAssoc as Eq isn't defined for functions...
-- instead use QuickCheck to generate a random value which is applied
-- to both combined functions producing a result which can be tested 
-- for equality
combineAssoc :: (Semigroup b, Eq b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc v a b c = (unCombine (a <> (b <> c)) $ v) == (unCombine ((a <> b) <> c)  $ v)

type CombineAssoc = Int -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

combineLeftIdentity :: (Eq b, Monoid b, Semigroup b) => a -> Combine a b -> Bool
combineLeftIdentity x a = (unCombine (mempty <> a) $ x) == (unCombine a $ x)

combineRightIdentity :: (Eq b, Monoid b, Semigroup b) => a -> Combine a b -> Bool
combineRightIdentity x a = (unCombine (a <> mempty) $ x) == (unCombine a $ x)

main :: IO ()
main =  do
  quickCheck (combineAssoc :: CombineAssoc)
  quickCheck (combineLeftIdentity :: Int -> Combine Int (Sum Int) -> Bool)
  quickCheck (combineRightIdentity :: Int -> Combine Int (Sum Int) -> Bool)

  counts <- runTestTT test1
  print counts