module SemiGroup5 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import ArbitrarySum

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup (Four a b c d) where
    x <> _ = x
    
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
   arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
   
type FourAssoc = Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Bool

main :: IO ()
main = 
  quickCheck (semigroupAssoc :: FourAssoc)