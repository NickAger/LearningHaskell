module Possibility where
  
import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorLaws
import Test.QuickCheck.Gen (oneof)

data Possibility a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)
  
instance Functor Possibility where
  fmap _ LolNope = LolNope
  fmap f (Yeppers m) = Yeppers (f m)
  
instance (Arbitrary a) => Arbitrary (Possibility a) where
   arbitrary = oneof [return LolNope, Yeppers <$> arbitrary]
   
type PossibilityFC = Possibility Int -> Fun Int String -> Fun String Int -> Bool

