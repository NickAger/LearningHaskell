module SemiGroup11 (
  Validation (Failure, Success)
  ) where
  
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import ArbitrarySum

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Failure x <> Failure y = Failure (x <> y)
    Failure x <> _ = Failure x
    _ <> v = v
    
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Validation a b) where
   arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]
   
   
type ValidationAssoc = Validation (Sum Int) Int -> Validation (Sum Int) Int -> Validation (Sum Int) Int -> Bool

main :: IO ()
main =  quickCheck (semigroupAssoc :: ValidationAssoc)
