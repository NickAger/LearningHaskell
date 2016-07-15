module Either where
  
import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorLaws
import Test.QuickCheck.Gen (oneof)
  
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight s@(Left _) = s


showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right n) = Right $ show n
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither m = fmap show m

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- making more generic dropping Either and replacing with a Functor

liftedInc :: (Num a, Functor f) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Show a, Functor f) => f a -> f String
liftedShow = fmap show

-- Exercise

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)
  
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
   arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]
  
type SumFC = Sum String Int -> Fun Int String -> Fun String Int -> Bool