module ExercisesLibraryFunctions where

import Prelude hiding(sum, product, elem, minimum, maximum, null, length, foldMap)
import Data.Foldable hiding(sum, product, elem, minimum, maximum, null, length, toList, foldMap, fold)
import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum f = foldr (+) 0 f

product :: (Foldable t, Num a) => t a -> a
product f = foldr (*) 1 f

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x f = foldr (\x' acc -> if x' == x then True else acc) False f

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum f = foldr compare Nothing f
  where
    compare x (Just acc) = if x < acc then Just x else Just acc
    compare x Nothing = Just x 

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum f = foldr compare Nothing f
  where
    compare x (Just acc) = if x > acc then Just x else Just acc
    compare x Nothing = Just x 

null :: (Foldable t) => t a -> Bool
null f = length f == 0

length :: (Foldable t) => t a -> Int
length f = foldr (\_ acc -> acc + 1) 0 f

toList :: (Foldable t) => t a -> [a]
toList f = foldr (:) [] f

-- hint foldMap :: Monoid m => (a -> m) -> t a -> m
fold :: (Foldable t, Monoid m) => t m -> m
fold f = foldMap id f

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldr (\x acc -> (f x) <> acc) mempty xs

-- an explanation of why "fmap length Just [1,2,3]" returns 1
{-
nickager: > length $ Just [1,2,3]
lambdabot:  1
nickager: > fmap length Just [1,2,3]
lambdabot:  1
merijn: wait
merijn: How is that not a type error?
nickager: > fmap length $ Just [1,2,3]
lambdabot:  Just 3
merijn: OH
merijn: yay
chipf0rk: because Functor of functions?
merijn: Function applicative 
nickager: I don’t understand but clearly others do ...
nickager: ?
chipf0rk: great fun. ugh, I dislike seeing that kind of thing in actual code.
lambdabot: Num b => Maybe b
chipf0rk: > length (Just "blargh")
nitrix: (When you have a value)
lambdabot:  1
chipf0rk: > length Nothing
lambdabot:  0
chipf0rk: this part is due to `Maybe` being a `Foldable`.
nickager: I understand the Foldable part of Maybe
chipf0rk: alright 
nickager: but not the  "instance Functor ((->) r) where"
chipf0rk: now we need to be aware that:
chipf0rk: fmap length Just [1,2,3] _is the same as_ (fmap length Just) [1,2,3]
chipf0rk: we could add more parentheses to the left if we were so inclined, but this is a lot easier to read 
chipf0rk: now what does "fmap length Just" mean?
nickager: > fmap length Just
lambdabot:     No instance for (Typeable a0)
lambdabot:       arising from a use of ‘show_M545752631476904532215204’
lambdabot:     In the expression:
chipf0rk: yeah, well. it's a function.
chipf0rk: both `length` and `Just` are functions 
LKoen: :t fmap length Just
lambdabot: a -> Int
chipf0rk: and the "functor of functions" is just function composition.
chipf0rk: fmap = (.)
chipf0rk: so: (fmap length Just) == (length . Just)
chipf0rk: (length . Just) [1,2,3] == length (Just [1,2,3])
chipf0rk: > length (Just [1,2,3])
lambdabot:  1
chipf0rk: the weirdness largely comes from this being a thing that means wildly different things depending on just one pair of parens or one ($)
chipf0rk: that's why I've become wary of seeing a foldr "with too many arguments" where someone is folding together a function from several functions, to then apply it to an argument.
chipf0rk: just. put. parens.
chipf0rk: (sorry, that's tangential)
chipf0rk: gives you easy switching between multiple GHC versions
nickager: chipf0rk: thanks for detailed the explanation
chipf0rk: nickager: Does it entirely make sense now, or is it still weird?
chipf0rk: np 
nickager: it is still a bit weird but hopefully with practice will become less odd
chipf0rk: I wonder if I've ever seen the functor of functions be useful and not just make things less readable
nickager: chipf0rk: what confuses me is that “fmap length Just [1,2,3]” works if a little confusingly, but “fmap length Just” gives a type error.
merijn: :t fmap length Just
lambdabot: a -> Int
merijn: No type error
merijn: It just doesn't print
chipf0rk: nickager: that's just because it's a function - what you saw earlier is a function not being printable
chipf0rk: i.e. not having a Show instance
merijn: :t length 'c' -- this is a type error
lambdabot:     Couldn't match expected type ‘t0 a0’ with actual type ‘Char’
lambdabot:     In the first argument of ‘length’, namely ‘'c'’
lambdabot:     In the expression: length 'c'
nickager: why does “fmap length Just” try to Show a function a not “1”
merijn: nickager: Because the result of "fmap length Just" is a function of type "a -> Int"
nitrix: nickager: instance Functor ((->) r) where fmap = (.)
merijn: nickager: And lambdabot automatically tries to show the result of an expression
chipf0rk: nickager: "Just" itself is just the constructor
merijn: > fmap
lambdabot:     No instance for (Typeable f0)
lambdabot:       (maybe you haven't applied enough arguments to a function?)
lambdabot:       arising from a use of ‘show_M550437742466743138115465’
chipf0rk: nickager: you can't calculate the length of just a "Just" (heh). It's not a Maybe value yet.
chipf0rk: it takes something and then makes an actual Maybe value from it.
chipf0rk: :t Just
lambdabot: a -> Maybe a
nickager: :t fmap length Just
lambdabot: a -> Int
chipf0rk: nickager: not that this even applies in that snippet, since `length` and `Just` are being composed. but it seems your thoughts were going to something like "length (Just ...)" which would indeed be 1, but that's far from being the same thing 
nickager: is a function which will take any parameter and always return 1
nickager: ?
chipf0rk: indeed
nickager: thanks everyone for you patient explanations
chipf0rk: nickager: you're welcome 
 
-}