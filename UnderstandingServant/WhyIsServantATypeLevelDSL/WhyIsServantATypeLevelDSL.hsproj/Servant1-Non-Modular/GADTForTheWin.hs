{-# LANGUAGE GADTs #-}
module GADTForTheWin where
  
import Debug.Trace
  
-- from: https://stackoverflow.com/questions/51875537/servant-like-implementation-with-gadts/51892552#51892552

type Link = [String]

data Method = Get | Post
  deriving Show

{-
previous definition

data Endpoint = Static String Endpoint
              | Capture Endpoint
              | Verb Method
              
:t Static :: String -> Endpoint -> Endpoint
:t Capture :: Endpoint -> Endpoint
:t Verb :: Method -> Endpoint

:t endpoint1 -- :: Endpoint
:t twoCaptures -- :: Endpoint
:t oneCapture -- :: Endpoint
-}


data Endpoint t where
   Static :: String -> Endpoint t -> Endpoint t
   Capture :: Endpoint t -> Endpoint (String -> t)
   Verb :: Method -> Endpoint Link

linkTo :: Endpoint t -> t
linkTo e = go e []
   where
   go :: Endpoint t -> Link -> t
   go (Static str rest) l = go rest (str : l)
   go (Capture rest)    l = \s -> go rest (s : l)
   go (Verb _method)    l = reverse l
   
linkToTrace :: Endpoint t -> t
linkToTrace  e = go e []
   where
   go :: Endpoint t -> Link -> t
   go (Static str rest) l = trace ("Static '" ++ show str ++ "' rest") $  go rest (str : l)
   go (Capture rest)    l = trace "Capture" $  \s -> go rest (s : l)
   go (Verb _method)    l = trace ("Verb '" ++ show _method ++ "', l = '" ++ show l ++ "'") $ reverse l

-- ++ show  _method ++ "', l = '" ++ show l ++ "'"

---

endpoint1 = Static "hello" (Verb Get) -- :: Endpoint Link
twoCaptures = Capture (Capture (Verb Post)) -- :: Endpoint (String -> String -> Link)
oneCapture = Static "hello" (Capture (Verb Get)) -- Endpoint (String -> Link)

{-



-}