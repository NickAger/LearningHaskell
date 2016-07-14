module SemiGroup6 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.HUnit

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    _ <> x = x
    
instance Arbitrary BoolConj where
   arbitrary = BoolConj <$> arbitrary 
   
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Expected:
-- (BoolConj True) <> (BoolConj True) == BoolConj True
-- (BoolConj True) <> (BoolConj False) == BoolConj False

test1 = TestCase (assertEqual "equal true propogated" (BoolConj True <> BoolConj True) (BoolConj True))
test2 = TestCase (assertEqual "any false propogated" (BoolConj True <> BoolConj False) (BoolConj False))

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  counts <- runTestTT (TestList [test1, test2])
  print counts