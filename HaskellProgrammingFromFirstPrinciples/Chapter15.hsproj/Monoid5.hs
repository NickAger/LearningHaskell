module Monoid5 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws
import Test.HUnit

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj True <> _ = BoolDisj True
    _ <> x = x
    
instance Monoid BoolDisj  where
  mempty = BoolDisj False
  mappend = (<>)
  
instance Arbitrary BoolDisj where
   arbitrary = BoolDisj <$> arbitrary 
   
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Expected:
-- BoolDisj True `mappend` mempty == BoolDisj True
-- mempty `mappend` BoolDisj False == BoolDisj False

test1 = TestCase (assertEqual "equal true propogated" (BoolDisj True `mappend` mempty) (BoolDisj True))
test2 = TestCase (assertEqual "any true propogated" (mempty `mappend` BoolDisj False) (BoolDisj False))

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  counts <- runTestTT (TestList [test1, test2])
  putStrLn(show counts)
  return ()