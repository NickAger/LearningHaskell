module SemiGroup10 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import Test.HUnit

newtype Comp a = Comp { unComp :: (a -> a) }


instance Semigroup (Comp a) where
    f1 <> f2 = Comp $ (unComp f1) . (unComp f2)
    
-- think this link could help: http://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions
--instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
--    arbitrary = ...
   
f = Comp $ \n -> (n + 1)
g = Comp $ \n -> (n - 1)

test1 = TestCase (assertEqual ""  (unComp (f <> g) $ 0) 0)
test2 = TestCase (assertEqual "" (unComp (f <> g) $ 1) 1)
test3 = TestCase (assertEqual "" (unComp (f <> f) $ 1) 3)
test4 = TestCase (assertEqual "" (unComp (g <> f) $ 1) 1)
   
type CompAssoc = Comp Int -> Comp Int -> Comp Int -> Bool

main :: IO ()
main =  do
--  quickCheck (semigroupAssoc :: CombineAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4])
  putStrLn(show counts)
  return ()