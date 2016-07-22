module Exercises17_9 where

{-

nickager: How can I ask in the repl for the type of (<*>) specialised for []
nickager: I tried: “:t (<*>) :: Applicative []”
kadoban: :t (<*>) `asAppliedTo` []
lambdabot: [a -> b] -> [a] -> [b]
merijn: asAppliedTo is a lambdabot thing, though 
kadoban: But you can reimplement it fairly trivially, right?
hexagoxel: @src asAppliedTo
lambdabot: f `asAppliedTo` a = f where _ = f a
lambdabot: infixl 0 `asAppliedTo`
nickager: that’s a handy trick thanks
nitrix: Even better:
nitrix: :t (<*>) @[]
nitrix: hexagoxel: nickager You'll need :set -XTypeApplications
-}

f `asAppliedTo` a = f where _ = f a
  
{-

kadoban: nickager: Note that you can /msg lambdabot and play with it there
Iceland_jack: nickager: the second argument of 'asAppliedTo' is not a type, but a term
Iceland_jack: :t (<*>) `asAppliedTo` (undefined, undefined)
lambdabot: Monoid t => (t, a -> b) -> (t, a) -> (t, b)
Iceland_jack: :t (<*>) `asAppliedTo` (undefined :: Sum Int, undefined)
lambdabot: (Sum Int, a -> b) -> (Sum Int, a) -> (Sum Int, b)
kadoban: So, /msg lambdabot > "hi" should work, for example

TODO: types vs terms
-}

--
 
-- from Chapter 22, perhaps the book meant something like:
{-
One of the nice things about Haskell is we can assert a more concrete type for functions like (<*>) and see if the compiler agrees we're putting force something hypothetically possible.
Let's remind ourselves of the type of (<*>):

Prelude> :t (<*>)
(<*>)::Applicative f=> f (a -> b) -> f a -> f b

-- in this case, we know f is ((->) a)
-- so we concretize it thusly
Prelude>:t(<*>)::(a->a->b)->(a->a)->(a->b)
(<*>)::(a->a->b)->(a->a)->(a->b)

The compiler agrees that this is a possible type for (<*>).
-}

-- UPDATE: I fairly sure this is what the book intended. I've updated the exercises accordingly.



