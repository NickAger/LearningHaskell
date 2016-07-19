module Exercises where
  
import Control.Monad
  

-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- alternative implementations
--j :: Monad m => m (m a) -> m a
--j x = do
--  x' <- x
--  x'

--j :: Monad m => m (m a) -> m a
--j x = x >>= id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x1 x2 = f <$> x1 <*> x2

-- alternative
--l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a x1 f = f <*> x1

-- alternatives
--a :: Monad m => m a -> m (a -> b) -> m b
--a = flip (<*>)

--a :: Monad m => m a -> m (a -> b) -> m b
--a = flip ap

-- 5. 

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:[]) f = (:[]) <$> f x
meh (x:xs) f = (:) <$> (f x) <*> meh (xs) f

-- alternatives
--meh :: Monad m => [a] -> (a -> m b) -> m [b]
--meh (x:[]) f = (:[]) <$> f x
--meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

--meh :: Monad m => [a] -> (a -> m b) -> m [b]
--meh [] f = pure []
--meh (x:xs) f = do
--  b <- f x
--  bs <-(meh xs f)
--  return (b:bs)

-- 6. 
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

