module Monoid2 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws
import ArbitrarySum

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)
    
instance (Semigroup a, Monoid a) => Monoid (Identity a)  where
  mempty = Identity mempty
  mappend = (<>)
    
instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = Identity <$> arbitrary
   
type IdentityAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)