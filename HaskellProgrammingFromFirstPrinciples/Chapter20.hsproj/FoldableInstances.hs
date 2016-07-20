module FoldableInstances where
 
import Data.Foldable
import Data.Monoid
import OptionalMonoid
  
data Identity a = Identity a 
-- We’re only obligated to write foldr or foldMap, but we’ll write both plus foldl just so you have the gist of it

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
  

--


instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Only x) = f x z
  
  foldl _ z Nada = z
  foldl f z (Only x) = f z x
  
  foldMap _ Nada = mempty
  foldMap f (Only a) = f a
  
  
  


  