module SemiGroup13 where
  
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import ArbitrarySum
import SemiGroup11

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)
  
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth(Success x) <> AccumulateBoth(Success y) = AccumulateBoth(Success (x <> y))
    AccumulateBoth(Failure x) <> AccumulateBoth(Failure y) = AccumulateBoth(Failure (x <> y))
    AccumulateBoth(Failure x) <> _ = AccumulateBoth(Failure x)
    _ <> v = v
    
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ AccumulateBoth(Failure x), return $ AccumulateBoth(Success y)]
   
   
type AccumulateBothAssoc = AccumulateBoth (Sum Int) (Sum Int) -> AccumulateBoth (Sum Int) (Sum Int) -> AccumulateBoth (Sum Int) (Sum Int) -> Bool

main :: IO ()
main =  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
