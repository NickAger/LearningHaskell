module TypedHoles where
  

-- from http://www.shimweasel.com/2015/02/17/typed-holes-for-beginners
{-
As @benno37 pointed out, it’s the underscore that makes it a typed hole. This is quite clever really - it is only considered a hole if it would otherwise be a scope error, so if for some misbegotten reason you decide to name a parameter to a function “_foo”, it will remain a normal variab
-}
g :: (a -> b) -> (a, c) -> (b, c)
g x y = _foo

g2 :: (a -> b) -> (a, c) -> (b, c)
g2 x y = (_foo, _bar)

g3 :: (a -> b) -> (a, c) -> (b, c)
g3 x y = (_foo, _bar y)

g4 :: (a -> b) -> (a, c) -> (b, c)
g4 x y = (_foo, snd y)

g5 :: (a -> b) -> (a, c) -> (b, c)
g5 x y = ( x _foo,snd y)

g6 :: (a -> b) -> (a, c) -> (b, c)
g6 x y = ( x (_foo y) ,snd y)

g7 :: (a -> b) -> (a, c) -> (b, c)
g7 x y = (x (fst y) ,snd y)