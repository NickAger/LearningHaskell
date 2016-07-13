module Semigroup1 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial
    
instance Arbitrary Trivial where
   arbitrary = return Trivial
   
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = 
  quickCheck (semigroupAssoc :: TrivialAssoc)
  