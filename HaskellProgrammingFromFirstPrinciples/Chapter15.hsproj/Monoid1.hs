module Monoid1 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import MonoidLaws

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial
    
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)
    
instance Arbitrary Trivial where
   arbitrary = return Trivial
   
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)