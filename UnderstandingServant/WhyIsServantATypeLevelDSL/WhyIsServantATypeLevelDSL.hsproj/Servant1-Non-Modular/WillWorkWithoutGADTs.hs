-- An experiment to see if I can make it work without GADTs
-- the experiment didn't work, but helped explain the GADT solution
module WillWorkWithoutGADTs where
  
import Debug.Trace
  

type Link = [String]

data Method = Get | Post
  deriving Show



data Endpoint = Static String Endpoint
              | Capture Endpoint
              | Verb Method
              

-- problem here is that `t` can't simulateously be a lambda and `[String]`
linkTo :: Endpoint -> t
linkTo e = go e []
   where
   go :: Endpoint -> Link -> t
   go (Static str rest) l = go rest (str : l)
   go (Capture rest)    l = \s -> go rest (s : l)
   go (Verb _method)    l = reverse l


---

endpoint1 = Static "hello" (Verb Get) -- :: Endpoint Link
twoCaptures = Capture (Capture (Verb Post)) -- :: Endpoint (String -> String -> Link)
oneCapture = Static "hello" (Capture (Verb Get)) -- Endpoint (String -> Link)

{-



-}