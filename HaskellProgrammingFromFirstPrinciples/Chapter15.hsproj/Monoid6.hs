module Monoid6 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws
import Test.HUnit
import ArbitrarySum

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    f1 <> f2 = Combine $ \n -> ((unCombine f1) n) <> ((unCombine f2 n))
    
instance (Semigroup b, Monoid b)=> Monoid (Combine a b)  where
  mempty = Combine $ \_ -> mempty
  mappend = (<>)
 
-- think this link could help: http://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions
--instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
--    arbitrary = ...
   
f = Combine $ \n -> Sum (n + 1)

test1 = TestCase (assertEqual ""  (unCombine (mappend f mempty) $ 1) (Sum 2))
   
type CombineAssoc = Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Bool

main :: IO ()
main =  do
--  quickCheck (semigroupAssoc :: CombineAssoc)
--  quickCheck (monoidLeftIdentity :: Combine (Sum Int) (Sum Int) -> Bool)
--  quickCheck (monoidRightIdentity :: Combine (Sum Int) (Sum Int) -> Bool)

  counts <- runTestTT (TestList [test1])
  putStrLn(show counts)
  return ()