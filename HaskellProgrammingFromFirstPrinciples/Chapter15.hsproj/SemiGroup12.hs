module SemiGroup12 where
  
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import ArbitrarySum
import SemiGroup11

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)
  
instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight(Success x) <> AccumulateRight(Success y) = AccumulateRight(Success (x <> y))
    AccumulateRight(Failure x) <> _ = AccumulateRight(Failure x)
    _ <> v = v
    
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ AccumulateRight(Failure x), return $ AccumulateRight(Success y)]
   
   
type AccumulateRightAssoc = AccumulateRight Int (Sum Int) -> AccumulateRight Int (Sum Int) -> AccumulateRight Int (Sum Int) -> Bool

main :: IO ()
main =  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
