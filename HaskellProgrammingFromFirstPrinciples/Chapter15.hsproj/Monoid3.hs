module Monoid3 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws
import ArbitrarySum

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)
    
instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b)  where
  mempty = Two mempty mempty
  mappend = (<>)
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
   arbitrary = Two <$> arbitrary <*> arbitrary
   
type TwoAssoc = Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) (Sum Int) -> Bool)