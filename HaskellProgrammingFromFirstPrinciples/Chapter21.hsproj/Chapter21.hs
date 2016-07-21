import Data.Maybe
import Data.Foldable
import Data.Monoid

{-
Reminder: in Chapter 18 we wrote the following:
(I weakened the contraint from Monad to Applicative)
-}
meh :: Applicative m => [a] -> (a -> m b) -> m [b]
meh (x:[]) f = (:[]) <$> f x
meh (x:xs) f = (:) <$> (f x) <*> meh (xs) f

flipType :: (Applicative m) => [m a] -> m [a]
flipType xs = meh xs id

-- note sequence in the prelude is defined as:
-- sequence :: Monad m => [m a] -> m [a]

{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
-}

-- sequenceA = traverse id
-- traverse f = sequenceA . fmap f

-- In versions of GHC prior to 7.10, the type of mapM was the following:
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- contrast with:
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
