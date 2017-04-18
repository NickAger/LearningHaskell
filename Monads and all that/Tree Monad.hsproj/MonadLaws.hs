module MonadLaws where
import Test.QuickCheck
import Test.QuickCheck.Function

type PropLeftUnit m = Integer -> Fun Integer (m Integer) -> Bool

prop_LeftUnit x (Fun _ f) = (return x >>= f) == f x

type PropRightUnit m = m Integer -> Bool

prop_RightUnit m = (m >>= return) == m

type PropAssoc m = m Integer ->
                   Fun Integer (m Integer) ->
                    Fun Integer (m Integer) ->
                    Bool
                    
prop_Assoc m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g)
  ==
  (m >>= \x -> f x >>= g)