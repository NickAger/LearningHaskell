module ReimplementingWeakerTypes where

import Data.Functor.Identity
import Data.Monoid
import Data.Functor.Constant

-- from "Strength for understanding"

edgelordMap :: Traversable t => (a -> b) -> t a -> t b
edgelordMap f t = runIdentity $ traverse (Identity . f) t

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = getConstant $ traverse (Constant . f) t