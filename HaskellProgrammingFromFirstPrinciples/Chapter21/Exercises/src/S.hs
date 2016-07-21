{-# LANGUAGE FlexibleContexts #-}

module S where

import Test.QuickCheck
import Test.QuickCheck.Checkers

import Data.Monoid

data S n a = S (n a) a
  deriving (Eq, Ord, Show)

{-

nickager: I’m trying to understand this type: “data S n a = S (n a) a”
nickager: is (n a) applying function n to parameter a?
PiDelport: nickager: It's taking n as a type constructor, and applying that to the type a
nickager: so N could be “Just”
nickager: for example?
PiDelport: Not quitee, but it could be Maybe
PiDelport: Maybe is the type constructor:
PiDelport: :k Maybe
lambdabot: * -> *
ralu: :k Either
lambdabot: * -> * -> *
PiDelport: Just is a data constructor.
PiDelport: nickager: In other words, Maybe taken a concrete type, and then returns a new type.
PiDelport: Such as "Maybe Int"
PiDelport: Or "Maybe String".
PiDelport: So one example instance of the *type* above is S Maybe Int
PiDelport: And a valid value of that type would be S (Just 5) 5
PiDelport: It's a bit confusing because S is being used both at the type level and value level there, but they're distinct.
PiDelport: You could also rewrite the above to data S n a = MkS (n a) a
nickager: another valid instance:
nickager: S Nothing "hello"
PiDelport: There, S is the type constructor, and MkS is the data constructor for it.
PiDelport: nickager: Right.
nickager: OK thanks that makes sense
PiDelport: And the type of that would be S Maybe String
-}

-- data S n a = S (n a) a
-- S (Just 5) 10

-- I cheated a little on this exercise and looked at https://github.com/dmvianna/haskellbook/blob/master/src/Ch21-Traversable.hs
instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

{-
> let s = S (Just "hello") "again"
> traverse (\x -> [x ++ "!!"]) s
[S (Just "hello!!") "again!!"]
-}
--

instance (Arbitrary (n a), CoArbitrary (n a),
          Arbitrary a, CoArbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq
