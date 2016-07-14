module Monoid4 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws
import Test.HUnit

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    _ <> x = x

instance Monoid BoolConj  where
  mempty = BoolConj True
  mappend = (<>)
    
instance Arbitrary BoolConj where
   arbitrary = BoolConj <$> arbitrary 
   

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Expected:
-- (BoolConj True) `mappend` mempty == BoolConj True
-- mempty `mappend` (BoolConj False) == BoolConj False

test1 = TestCase (assertEqual "true + identify = true" (BoolConj True `mappend` mempty) (BoolConj True))
test2 = TestCase (assertEqual "identity + false = false" (mempty `mappend` BoolConj False) (BoolConj False))

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  counts <- runTestTT (TestList [test1, test2])
  putStrLn(show counts)