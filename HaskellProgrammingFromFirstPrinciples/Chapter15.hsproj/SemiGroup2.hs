module SemiGroup2 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import ArbitrarySum

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)
    
instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = do
    x <- arbitrary 
    return (Identity x)
   
type IdentityAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

main :: IO ()
main = 
  quickCheck (semigroupAssoc :: IdentityAssoc)
  
