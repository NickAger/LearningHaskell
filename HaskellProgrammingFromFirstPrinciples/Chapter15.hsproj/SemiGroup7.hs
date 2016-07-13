module SemiGroup7 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj True <> _ = BoolDisj True
    _ <> x = x
    
instance Arbitrary BoolDisj where
   arbitrary = BoolDisj <$> arbitrary 
   
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Expected:
-- (BoolDisj True) <> (BoolDisj True) == BoolConj True
-- (BoolDisj True) <> (BoolDisj False) == BoolConj True

test1 = TestCase (assertEqual "equal true propogated" (BoolDisj True <> BoolDisj True) (BoolDisj True))
test2 = TestCase (assertEqual "any true propogated" (BoolDisj True <> BoolDisj False) (BoolDisj True))

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  counts <- runTestTT (TestList [test1, test2])
  putStrLn(show counts)
  return ()