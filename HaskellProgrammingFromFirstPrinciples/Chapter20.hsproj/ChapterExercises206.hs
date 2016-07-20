module ChapterExercises206 where
  
import Data.Foldable
import Data.Monoid

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- {-# MINIMAL foldMap | foldr #-}
-- fold :: (Monoid m, Foldable t) => t m -> m

-- 1 Constant

data Constant a b = 
  Constant a deriving Show
  
instance Foldable (Constant x) where
  foldr _ b _ = b
  foldMap _ _ = mempty

-- 2 

data Two a b = Two a b deriving Show

instance Foldable (Two a) where
  foldr f b (Two _ a) = (f a b)

-- 3

data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldr f b (Three _ _ c) = (f c b)
  
-- 4

data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldr f b (Three' _ b' b'') = (f b'' (f b' b)) 
  
-- 5

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f b (Four' _ b' b'' b''') = (f b''' (f b'' (f b' b))) 

-- filterF

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldr filter' mempty xs
  where 
    filter' x acc = if (f x) then acc <> (pure x) else acc
    
oddSum :: Integral a => Sum a -> Bool
oddSum (Sum x) = odd x


filterF' :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' f xs = foldMap mapToApplicative xs
  where
    mapToApplicative x = if (f x) then (pure x) else mempty



 