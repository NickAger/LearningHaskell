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

 