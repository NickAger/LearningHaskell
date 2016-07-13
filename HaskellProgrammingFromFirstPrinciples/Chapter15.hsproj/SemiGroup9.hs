module SemiGroup9 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import Test.HUnit

newtype Combine a b = Combine { unCombine :: (a -> b) }


instance Semigroup b => Semigroup (Combine a b) where
    f1 <> f2 = Combine $ \n -> ((unCombine f1) n) <> ((unCombine f2 n))
 
-- think this link could help: http://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions
--instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
--    arbitrary = ...
   
f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

test1 = TestCase (assertEqual ""  (unCombine (f <> g) $ 0) (Sum 0))
test2 = TestCase (assertEqual "" (unCombine (f <> g) $ 1) (Sum 2))
test3 = TestCase (assertEqual "" (unCombine (f <> f) $ 1) (Sum 4))
test4 = TestCase (assertEqual "" (unCombine (g <> f) $ 1) (Sum 2))
   
type CombineAssoc = Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Bool

main :: IO ()
main =  do
--  quickCheck (semigroupAssoc :: CombineAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4])
  putStrLn(show counts)
  return ()