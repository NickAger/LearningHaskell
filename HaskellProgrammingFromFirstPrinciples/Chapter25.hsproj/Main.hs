{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ApplicativeDo #-}
import Control.Applicative


newtype Identity a = Identity {runIdentity :: a}
     
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- kind:- Compose :: (* -> *) -> (* -> *) -> * -> *
-- type:- (.) :: (b -> c) -> (a -> b) -> a -> c 

-- Functors

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
   

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa
 
 

-- Applicatives

{-
I asked on 'haskell-beginners' when I struggled to derive 'pure' and `<*>` below:

merijn: nickager: So, we have "pure x = Compose $ ..." which should have type 'Compose f g a'
nickager: the type of Compose is:
nickager: Compose :: f (g a) -> Compose f g a
merijn: nickager: Right, so we need an 'f' with 'g a' inside it
jle`: you can give the explicit type signatures but you'll need to enable ScopedTypeVariables
jle`: pure x = Compose ((pure x :: g a) :: f (g a))
Akii: I got `pure = Compose . pure . pure`
Akii: also struggling with <*> tho 
nickager: I’m afraid I cheated (https://wiki.haskell.org/Applicative_functor) and came up with:
nickager:  (Compose fgatob) <*> (Compose fga) = Compose $ liftA2 (<*>) fgatob fga
nickager: where liftA2 f x = (<*>) (fmap f x)
Akii: I got `Compose $ fmap (<*>) f <*> a`
Akii: :t fmap (<*>)
lambdabot: (Functor f1, Applicative f) => f1 (f (a -> b)) -> f1 (f a -> f b)
nickager: Akii: I working through trying to understand the type signatures with values: ‘(Compose $ Just (Just (+1))) <*> (Compose $ Just (Just 1))’
Xion___: nickager: The <*> on the right size is for different Applicative: Maybe, not Compose. So you have a nested functor Compose . Maybe, and liftA2 (<*>) essentially makes <*> from a "single nested" wrapped application (f (a -> b) -> f a -> f b)) to a "double nested" one (f (f' (a -> b)) -> f (f' a) -> f (f' b))
Xion___: (note how both of these are just (a -> b) -> a -> b (i.e. regular ($)) with progressively more f-wrapping on each argument
Xion___: )
-} 

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgatob) <*> (Compose fga) = Compose $ liftA2 (<*>) fgatob fga
  

-- Looking at Martin's solution - https://github.com/martinrist/haskell-sandbox/blob/master/app/programmingHaskell/chapter25/notes.md

--instance (Applicative f, Applicative g) =>
--         Applicative (Compose f g) where
--
--    pure :: a -> Compose f g a
--    pure a = Compose $ pure (pure a)
--
--    (<*>) :: Compose f g (a -> b)
--          -> Compose f g a
--          -> Compose f g b
--    (Compose fgab) <*> (Compose fga) =
--        Compose $ (<*>) <$> fgab <*> fga
        
-- Compose Foldable
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap = undefined
  
-- Compose Traverable
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse = undefined
  
 
  