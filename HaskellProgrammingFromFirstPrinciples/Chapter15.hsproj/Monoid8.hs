module Monoid8 where
  
import Data.Monoid
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit
import MonoidLaws
import Text.Show.Functions
import ArbitrarySum

newtype Mem s a = Mem { runMem :: s -> (a,s) } deriving Show

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
  
    
{-
Iceland_jack:     arbitrary :: (CoArbitrary s, Arbitrary s, Arbitrary a) => Gen (s -> (a, s))
Iceland_jack: You can already generate functions like that
Iceland_jack: So you can just do
Iceland_jack: Mem <$> arbitrary
Iceland_jack:   :: (Arbitrary a, Arbitrary s, CoArbitrary s) => Gen (Mem s a)
Iceland_jack: Where 's' appears as an "input" (CoArbitrary, contravariant position) as well as an "output" (Arbitrary, covariant position)
Iceland_jack: that's why we get (Arbitrary s, CoArbitrary s)
-}
instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary
  
f' = Mem $ \s -> ("hi", s + 1)

test1 = TestCase (assertEqual "" (runMem (f' <> mempty) $ 0) ("hi", 1))
test2 = TestCase (assertEqual "" (runMem (mempty <> f') 0) ("hi", 1))
test3 = TestCase (assertEqual "" (runMem mempty 0 :: (String, Int)) ("", 0))
test4 = TestCase (assertEqual "" (runMem (f' <> mempty) 0) (runMem f' 0))
test5 = TestCase (assertEqual "" (runMem (mempty <> f') 0) (runMem f' 0))

-- can't use semigroupAssoc as Eq isn't defined for functions...
-- instead use QuickCheck to generate a random value which is applied
-- to both combined functions producing a result which can be tested 
-- for equality
memAssoc :: (Eq s, Eq a, Monoid a) => s -> Mem s a -> Mem s a -> Mem s a -> Bool
memAssoc v a b c = (runMem (a <> (b <> c)) $ v) == (runMem ((a <> b) <> c)  $ v)

type MemAssoc = Int -> Mem Int (Sum Int) -> Mem Int (Sum Int) -> Mem Int (Sum Int) -> Bool

memLeftIdentity :: (Eq s, Eq a, Monoid a) => s -> Mem s a -> Bool
memLeftIdentity x a = (runMem (mempty <> a) $ x) == (runMem a $ x)

memRightIdentity :: (Eq s, Eq a, Monoid a) => s -> Mem s a -> Bool
memRightIdentity x a = (runMem (a <> mempty) $ x) == (runMem a $ x)

main :: IO ()
main =  do
  quickCheck (memAssoc :: MemAssoc)
  quickCheck (memLeftIdentity :: Int -> Mem Int (Sum Int) -> Bool)
  quickCheck (memRightIdentity :: Int -> Mem Int (Sum Int) -> Bool)
    
  counts <- runTestTT (TestList [test1, test2, test3, test4, test5])
  print counts