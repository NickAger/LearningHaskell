module SemiGroup4 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import ArbitrarySum

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
    x <> _ = x
    
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
   arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
   
type ThreeAssoc = Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Bool

main :: IO ()
main = 
  quickCheck (semigroupAssoc :: ThreeAssoc)