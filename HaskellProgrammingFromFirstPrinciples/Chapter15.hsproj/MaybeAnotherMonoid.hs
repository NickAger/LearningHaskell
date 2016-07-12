module MaybeAnotherMonoid where

import OptionalMonoid
import MonoidAssociationLaws
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = firstMappend

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' Nada) f = f
firstMappend f (First' Nada) = f
firstMappend f _ = f

genFirst' :: (Arbitrary a) => Gen(First' a)
genFirst' = do
  x <- arbitrary
  frequency [(3, return (First' (Only x)))
            ,(1, return (First' Nada)) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

type FirstMappend = First' String -> First' String -> First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
