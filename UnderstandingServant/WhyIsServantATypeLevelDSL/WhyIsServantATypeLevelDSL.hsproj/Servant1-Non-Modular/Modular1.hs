{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

-- see https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html

module Modular1 where
  
import Data.List
  
-- static path fragments
data Static = Static String

-- variable path fragments ("captures")
data Capture = Capture

-- HTTP method
data Method = Get | Post
-- Leaf of a chain of :>'s, specifies the HTTP method
data Verb = Verb Method


-- chain a few "endpoint components" with this operator,
-- all chains must be terminated with a 'Verb' component.
infixr 5 :>
data a :> b = a :> b

-- a class to specify all the valid endpoint descriptions
class Endpoint a

-- Verb alone is one.
instance Endpoint Verb

-- if we have a valid description, sticking 'Static :>' in front of it
-- yields another valid description.
instance Endpoint rest => Endpoint (Static :> rest)

-- if we have a valid description, sticking 'Capture :>' in front of it
-- yields another valid description.
instance Endpoint rest => Endpoint (Capture :> rest)

-- GET /hello
endpoint1 = Static "hello" :> Verb Get
twoCaptures = Capture :> Capture :> Verb Post
oneCapture = Static "hello" :> Capture :> Verb Get

type Link = [String]

-- @renderLink ["hello", "world"] == "/hello/world"@
renderLink :: Link -> String
renderLink xs = '/' : intercalate "/" xs

class HasLink1 endpoint where
  -- return the path components
  link :: endpoint -> [String]

instance HasLink1 api => HasLink1 (Static :> api) where
  link (Static s :> api) = s : link api

instance HasLink1 api => HasLink1 (Capture :> api) where
  link (Capture :> api) = "<replaceWithCaptureVariableName>" : link api

instance HasLink1 Verb where
  link _ = []
  
--

class HasLink2 endpoint where
  type LinkType endpoint :: Type
  link2 :: endpoint -> LinkType endpoint

instance HasLink2 Verb where
  type LinkType Verb = Link
  link2 _ = []

instance HasLink2 api => HasLink2 (Static :> api) where
  type LinkType (Static :> api) = LinkType api
  link2 (Static s :> api) = s : link api

instance HasLink2 api => HasLink2 (Capture :> api) where
  -- HERE! we introduce a String argument
  type LinkType (Capture :> api) = String -> LinkType api

  -- we expand the type of link:
  -- link :: (Capture :> api) -> String -> LinkType api
  -- we see that our little `LinkType` trick there allows
  -- link to receive arguments when appropriate
  link2 (Capture :> api) captureValue = captureValue : link api