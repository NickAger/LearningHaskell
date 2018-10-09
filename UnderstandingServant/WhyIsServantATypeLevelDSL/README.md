# Why is Servant a type-level DSL?
* From: https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html

Attempt to replicate the web server below, but with strongly-type routes and the ability to minimise the repeatative error-prone boilerplate normally inherent in a web API:

```
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (split)
import Web.Scotty

main :: IO ()
main = scotty 8000 $
  get "/repeat/:n" $ do
    n <- param "n"
    json (replicate n n)

  post "/message" $ do
    msg <- jsonData
    json (split "\n" msg)
```

Ideally we'd just end up with the two functions below without all the normal boiler-plate:

```
-- Client for the first endpoint.
--
-- The Int is the value you want to set ":n" to (/repeat/23, /repeat/10, ...).
getRepeat :: Int -> ClientMonad [Int]

-- Client for the second endpoint.
--
-- The JSON body (just a naked string) to send is the Text argument.
postMessage :: Text -> ClientMonad [Text]
```



## See also
* https://github.com/NickAger/LearningHaskell/wiki/Servant

