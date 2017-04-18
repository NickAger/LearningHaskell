{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyList where
  
import Control.Monad
import Test.QuickCheck
import MonadLaws

newtype MyList a = MkList {unList :: [a]}
  deriving (Eq, Show, Arbitrary)
  
prop_MyListLeftUnit  = prop_LeftUnit  :: PropLeftUnit MyList
prop_MyListRightUnit = prop_RightUnit :: PropRightUnit MyList
prop_MyListAssoc     = prop_Assoc     :: PropAssoc MyList

mymap f (MkList xs) = MkList (map f xs)

flat :: MyList (MyList a) -> MyList a
flat (MkList xs) = MkList (concat (map unList xs))


instance Functor MyList where
  fmap = mymap
  
instance Applicative MyList where
  pure x = MkList [x]
  fs <*> xs = flat (mymap (\f -> mymap f xs) fs)

instance Monad MyList where
  xs >>= f = flat $ mymap f xs   

