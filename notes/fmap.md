> Functor is a means of lifting functions over structure so that we may transform only the contents, leaving the structure alone.

and

> fmap lifts functions into a structure

```haskell
let lms = [Just "Ave", Nothing, Just "woohoo"]
let replaceWithP = const 'p'
```

so ...
```haskell
replaceWithP lms                      -> 'p'
fmap replaceWithP lms                 -> 'ppp'
(fmap.fmap) replaceWithP lms          -> [Just 'p',Nothing,Just 'p']
(fmap.fmap.fmap) replaceWithP lms     -> [Just 'ppp',Nothing, Just 'pppppp']
```

