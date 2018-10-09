module NonModular1 where
  
-- see https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html

data Method = Get | Post

data Endpoint = Static String Endpoint
              | Capture Endpoint
              | Verb Method

-- GET /hello/:name
getHello :: Endpoint
getHello = Static "hello" (Capture (Verb Get))



-- note :> didn't work 
-- See: https://stackoverflow.com/questions/48600930/invalid-type-signature-for-in-haskell?rq=1
-- Symbols starting with : are reserved for constructors (data MyData a b = a :> b is a valid alternative
infixr 5 !>
(!>) :: (Endpoint -> Endpoint) -> Endpoint -> Endpoint
f !> x = f x

getHelloNew :: Endpoint
getHelloNew = Static "hello" !> Capture !> Verb Get

-- now generate a link from an Endpoint:

type Link = [String]

linkTo :: Endpoint -> Link
linkTo (Static str rest) = str : linkTo rest
linkTo (Verb _method)    = []
linkTo (Capture rest)    = "<replaceWithCaptureVariableName>" : linkTo rest

endpoint1 = Static "hello" !> Verb Get
oneCapture = Static "hello" !> Capture !> Verb Get
twoCaptures = Capture !> Capture !> Verb Post

noCaptures :: Endpoint
noCaptures = Static "hello" !> Verb Post

linkWithCapturesTo :: [String] -> Endpoint -> Link
linkWithCapturesTo captures (Static str rest) = str : linkWithCapturesTo captures rest
linkWithCapturesTo _ (Verb _method)    = []
linkWithCapturesTo (c:cs) (Capture rest)    = c : linkWithCapturesTo cs rest

{-
Indeed, we would like the type of linkTo to be Endpoint -> Link, Endpoint -> String -> Link, Endpoint -> String -> String -> Link and so on depending on what the Endpoint argument is. In other words, we want the return type of linkTo (when really seen as a function of one argument, which it is anyway) to depend on the value of type Endpoint it gets as input. That is, we want a type that depends on a value, i.e dependent types.
-}

{-
Nick Comment:
Presumably we want explicit number of parameters in the `linkTo` rather than my array solution in `linkWithCapturesTo` so that we can type check at compile time rather than have a runtime error if the length of the array doesn't match the number of captures.
-}

{-
Then I noticed they had implemented a similar function with the comment:

This solution is very unsatisfactory, first and foremost because it is possible for it to error out if we don’t supply the right number of captures. However, it will also silently let us pass too many capture values without telling us that some of them are not used. Such a function should be total, we really don’t want an implementation that can let us down if we’re not very careful.
-}

-- linkTo assumes that the capture values are given in the same order as we want
-- them to appear in the url.
linkTo2 :: Endpoint -> [String] -> Link
linkTo2 (Static str rest) captureValues  = str : linkTo2 rest captureValues
-- when there is at least one value left in the list, we use it.
-- when there isn't... we error out.
linkTo2 (Capture rest)    (c : cs)       = c   : linkTo2 rest cs
linkTo2 (Capture rest)    []             = error "linkTo: capture value needed but the list is empty" -- :-(
linkTo2 (Verb method)     _              = []
