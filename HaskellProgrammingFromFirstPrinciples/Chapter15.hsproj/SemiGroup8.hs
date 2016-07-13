module SemiGroup8 where
  
import Data.Semigroup
import Test.QuickCheck
import SemiGroupAssociativeLaw
import Test.QuickCheck.Gen (oneof)
import Test.HUnit

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)


instance Semigroup (Or a b) where
    Snd x <> _ = Snd x
    _ <> x = x
    
instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Or a b) where
   arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]
   

test1 = TestCase (assertEqual "right snd propogated" (Fst 1 <> Snd 2) (Snd 2))
test2 = TestCase (assertEqual "right fst propogated" (Fst 1 <> Fst 2) (Fst 2))
test3 = TestCase (assertEqual "Snd propogated" (Snd 1 <> Fst 2) (Snd 1))
test4 = TestCase (assertEqual "left Snd propogated" (Snd 1 <> Snd 2) (Snd 1))
   
type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

main :: IO ()
main =  do
  quickCheck (semigroupAssoc :: OrAssoc)
  counts <- runTestTT (TestList [test1, test2, test3, test4])
  putStrLn(show counts)
  return ()