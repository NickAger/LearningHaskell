module SemiGroup3 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import ArbitrarySum

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
   arbitrary = do
    x <- arbitrary 
    y <- arbitrary
    return (Two x y)
   
type TwoAssoc = Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Bool

main :: IO ()
main = 
  quickCheck (semigroupAssoc :: TwoAssoc)
