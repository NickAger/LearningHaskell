module Monoid8 where
  
import Data.Monoid
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit
import MonoidLaws

newtype Mem s a = Mem { runMem :: s -> (a,s) }

mappendMem :: Monoid a => Mem s a -> Mem s a -> Mem s a
mappendMem f1 f2 =
  Mem $ combiningFunc 
    where
      combiningFunc s =
        let 
          (a1, s1) = (runMem f1) s
          (a2, s2) = (runMem f2) s1
        in
          (a1 `mappend` a2, s2)
    
instance Monoid a => Monoid (Mem s a)  where
  mempty = Mem $ \n -> (mempty, n) 
  mappend = mappendMem
    
-- think this link could help: http://stackoverflow.com/questions/16214093/how-to-generate-random-typed-functions
--instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
--    arbitrary = ...
   
f' = Mem $ \s -> ("hi", s + 1)

test1 = TestCase (assertEqual ""  (runMem (f' <> mempty) $ 0) ("hi", 1))
test2 = TestCase (assertEqual "" (runMem (mempty <> f') 0) ("hi", 1))
test3 = TestCase (assertEqual "" (runMem mempty 0 :: (String, Int)) ("", 0))
test4 = TestCase (assertEqual "" (runMem (f' <> mempty) 0) (runMem f' 0))
test5 = TestCase (assertEqual "" (runMem (mempty <> f') 0) (runMem f' 0))

type MemAssoc = Mem Int (Sum Int) -> Mem Int (Sum Int) -> Mem Int (Sum Int) -> Bool

main :: IO ()
main =  do
--  quickCheck (semigroupAssoc :: MemAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4, test5])
  print counts
  return ()