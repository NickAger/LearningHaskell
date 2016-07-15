module InstancesOfFunctorExercises where
  
-- Implement Functor instances for the following datatypes. Use the QuickCheck properties we just showed you to validate them.

-- Exercise 1:
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
quickCheck (functorCompose' :: IntFC)

--instance  Functor Maybe  where
--    fmap _ Nothing       = Nothing
--    fmap f (Just a)      = Just (f a)
