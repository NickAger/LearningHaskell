{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyList2 where
  
import Control.Monad
import Test.QuickCheck
import MonadLaws

newtype MyList2 a = MkList2 {unList :: [a]}
  deriving (Show, Arbitrary)
  
instance Eq a => Eq (MyList2 a) where
  MkList2 xs == MkList2 ys = take 100 xs == take 100 ys
  
prop_MyListLeftUnit  = prop_LeftUnit  :: PropLeftUnit MyList2
prop_MyListRightUnit = prop_RightUnit :: PropRightUnit MyList2
prop_MyListAssoc     = prop_Assoc     :: PropAssoc MyList2

mymap f (MkList2 xs) = MkList2 (map f xs)

flat :: MyList2 (MyList2 a) -> MyList2 a
flat (MkList2 xs) = MkList2 (concat (map unList xs))

diag :: MyList2 (MyList2 a) -> MyList2 a
diag (MkList2 (MkList2 (x:xs):xss)) =
    mycons x (diag (MkList2 (map mydrop xss)))
diag _ = MkList2 []

mycons x (MkList2 xs)   = MkList2 (x:xs)

mydrop (MkList2 (x:xs)) = MkList2 xs
mydrop (MkList2 [])     = MkList2 []


instance Functor MyList2 where
  fmap = mymap
  
instance Applicative MyList2 where
  pure x = MkList2 (repeat x)
  fs <*> xs = diag (mymap (\f -> mymap f xs) fs)

instance Monad MyList2 where
  xs >>= f = diag $ mymap f xs 
  
{--
prop_Assoc m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g)
  ==
  (m >>= \x -> f x >>= g)
--}



-- *** Failed! Falsifiable (after 22 tests and 41 shrinks): 
-- MkList2 {unList = [0,11]}
-- {11->MkList2 {unList = [-10,0]}, _->MkList2 {unList = [0]}}
-- {-10->MkList2 {unList = []}, _->MkList2 {unList = [0,0]}}
 

