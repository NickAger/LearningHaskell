module MonoidExamples where

import Data.Monoid

test m = "hello" ++ m

-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids

-- class Monoid m where  
--    mempty :: m  
--    mappend :: m -> m -> m  
--    mconcat :: [m] -> m  
--    mconcat = foldr mappend mempty


--class  Functor f  where
--    fmap        :: (a -> b) -> f a -> f b


--class Monoid a where
--        mempty  :: a
--        mappend :: a -> a -> a
--        mconcat :: [a] -> a
--        mconcat = foldr mappend mempty

--First of all, we see that only concrete types can be made instances of Monoid, because the m in the type class definition doesn't take any type parameters. This is different from Functor and Applicative, which require their instances to be type constructors which take one parameter.

-- explanation:
-- Functor Maybe
-- fmap :: (String -> Int) -> Maybe String -> Maybe Int

-- Maybe is a type constructor which takes one parameter


-- a detour into Applicative:
-- By using <$>, the applicative style really shines, because now if we want to apply a function f between three applicative functors, we can write f <$> x <*> y <*> z. If the parameters weren't applicative functors but normal values, we'd write f x y z.

-- ie
-- f <$> x <*> y <*> z
-- if they were normal values instead you'd write:
-- f x y z


