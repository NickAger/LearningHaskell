module Tree where
  
import Control.Monad
import Test.QuickCheck
import MonadLaws
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)
  
instance Arbitrary a => Arbitrary (Tree a) where 
  arbitrary = frequency [(2,liftM Leaf arbitrary),
                         (1,liftM2 Branch arbitrary arbitrary)]
  shrink (Branch l r) =
    [l,r]++map (Branch l) (shrink r)++map (`Branch`r) (shrink l)
  shrink (Leaf a) = map Leaf (shrink a)

---
  
instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch x1 x2) = Branch (f <$> x1) (f <$> x2)
  
instance Applicative Tree where
  (Leaf f) <*> (Leaf x) = Leaf $ f x
  (Branch f1 f2) <*> (Branch x1 x2) = Branch (f1 <*> x1) (f2 <*> x2)
    
  pure x = Leaf x
  
instance Monad Tree where
    (Leaf x) >>= k = k x
    (Branch x y) >>= k = Branch (x >>= k) (y >>= k)

---

prop_TreeLeftUnit = prop_LeftUnit :: PropLeftUnit Tree
prop_TreeRightUnit = prop_RightUnit :: PropRightUnit Tree
prop_TreeAssoc = prop_Assoc :: PropAssoc Tree