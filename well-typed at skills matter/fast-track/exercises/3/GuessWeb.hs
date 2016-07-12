-- GuessWeb
--

--------------------------------------------------------------------------
-- Introduction:

-- Before looking at this module, you probably want to look at
-- Guess.hs first.  That module implements a little number-guessing
-- game that we try to move to the web here.
--
-- In order to compile this module or load it into GHCi, you need
-- a library that isn't part of the Haskell platform. In order to
-- install it, you should run
--
-- cabal update
-- cabal install httpd-shed-0.4
--
-- The first command fetches the latest package list from Hackage.
-- The second will download and install the package, which contains
-- a minimalistic web server.
--
-- Before we start, we need to enable one minor language extension
-- which is needed by the UrlParser library we also use:

{-# LANGUAGE TypeOperators #-}

-- Now we import all the modules we need:

import Data.Maybe
import Network.Shed.Httpd
import Network.URI
import Text.Html hiding ((</>))
import UrlParser

-- We also import our own module that implements the number-guessing
-- game:

import Guess

--------------------------------------------------------------------------
-- Web server setup:

-- Here, we set up the httpd-shed web server. This web server is ideal
-- for teaching purposes, because it's extremely simplistic.
-- There are a number of far more sophisticated Haskell web server
-- packages and web programming frameworks, if a more robust, flexible
-- or high-performance environment is required.

-- We use a hardcoded port number, 8080, to run the web server.

port :: Int
port = 8080

-- The main program immediately starts the server. At this point, try
-- to run 'main' from GHCi, open up a web browser at
--
-- http://localhost:8080/
--
-- and see if you can see a response from the server. Depending on
-- your firewall settings, you might have to configure an exception.

main :: IO ()
main = initServer port (server dumb)

-- The server itself is implemented as a function from requests to
-- responses that may perform IO. Both 'Request' and 'Response'
-- are types defined in the httpd-shed package. In this function
-- we extract GET requests, extract the URL they request, and
-- feed that to the 'fromURL' function we define below. In case
-- that function fails or for all other requests, we return a 404
-- error code. In case 'fromURL' succeeds, it always returns a value
-- of type 'Html', which we then render to a string and serve with
-- a 200 status message.

server :: Solver -> Request -> IO Response
server solver (Request { reqMethod = "GET", reqURI = uristr }) =
  return $ case fromURL (uriPath uristr) of
    Nothing  -> Response 404 [] "page not found"
    Just loc -> let resource = route solver loc
                in  Response 200 [] (renderHtml resource)
server _ r = return $ Response 404 [] "page not found"

--------------------------------------------------------------------------
-- Typed URLs:

-- We want to map our game to the web server. Therefore, we see the
-- 'GameState' type as a *typed* representation of the locations we have
-- to offer, and we explicitly map between these typed representations
-- and the strings. For implementing the mappings, we make use of the
-- UrlParser library that we have imported above. That library
-- provides a type 'UrlParser' which encodes a bidirectional mapping
-- between a String and a typed representation.
--
-- The mapping we're using is implemented below in 'stateParser'. The
-- following two functions extract the two conversion functions.
--
-- The function 'fromURL' maps a String to a State, but it can fail.

fromURL :: String -> Maybe GameState
fromURL str = parse1 stateParser str

-- The Maybe datatype is defined as follows:
--
-- data Maybe a = Nothing | Just a

-- The function 'toURL' maps a State back to a String, and it always
-- succeeds.

toURL :: GameState -> String
toURL st = fromJust (unparse1 stateParser st)

-- This function explains which URLs we want to use for which states.
--
-- Think of (<>) as an operator encoding choice, and of (<.>) as an
-- operator encoding sequence.

stateParser :: UrlParser r (GameState :- r)
stateParser =
    liftContinue (lit "/" <.> lit "continue" <.> lit "/" <.> int <.> lit "/" <.> int <.> lit "/" <.> int)
 <> liftDone     (lit "/" <.> lit "done"     <.> lit "/" <.> int)
 <> liftStart    (lit "/")

-- The first line above encodes URLs of the form "/continue/m/n/r"
-- where m, n and r are integers.
--
-- The second line encodes URLs of the form "/done/n" where n is
-- an integer.
--
-- The third line just covers "/".
--
-- The functions 'liftContinue', 'liftDone', and 'liftStart' try to
-- map the raw data contained in the URLs (which is three integers in
-- the first case, one integer in the second, and nothing in the
-- third) to the constructors in the 'GameState' datatype, and back.
--
-- Before we continue, actually try the following in GHCi:
--
-- fromURL "/continue/1/100/50"
-- fromURL "/done/20"
-- fromURL "/"
-- fromURL "/something"
-- toURL (Done 28)
-- toURL (Continue (Q (3,17) 5))

-- You can skip ahead to "Routing and Resources" below if you find the
-- following part too technical and tricky for now.
--
-- The type of UrlParser is a form of transformation between two
-- states that you can think of as stacks, where 'r' always represents
-- the unknown tail of the stack. Some UrlParsers such as 'int' parse
-- data and provide it, whereas others such as 'lit' don't. The type
-- of 'lit' is
--
-- lit :: String -> UrlParser r r
--
-- indicating that 'lit' leaves that stack unchanged. On the other
-- hand,
--
-- int :: UrlParser r (Int :- r)
--
-- indicates that it pushes an 'Int' on the stack. Keeping track of
-- this in the type system enables us to rule out sequencings of
-- parsers where we "forget" or "invent" data.
--
-- If we sequence parsers with (<.>), their effects on the stack are
-- combined:
--
-- (<.>) :: UrlParser b c -> UrlParser a b -> UrlParser a c
--
-- The type is very similar to that of function composition, which is
-- why we use a similar symbol (function composition is (.) in
-- Haskell).
--
-- Try this in GHCi:
--
-- :t int . int
--
-- Parsing two ints will push two 'Int' on the stack, which can
-- be seen in the type.
--
-- The first line in 'stateParser' above pushes three 'Int' values
-- on the stack, which we aim to transform into a 'GameState'. We do
-- this by means of 'liftContinue':

liftContinue :: UrlParser r (Int :- Int :- Int :- r) -> UrlParser r (GameState :- r)
liftContinue = xmap toContinue fromContinue
  where
    toContinue (lo :- hi :- g :- r) = Continue (Q (lo, hi) g) :- r
    fromContinue (Continue (Q (lo, hi) g) :- r) = Just (lo :- hi :- g :- r)
    fromContinue _                              = Nothing

-- Because our parsers are bidirectional (i.e., they provide a
-- serializer at the same time), we have to provide two mapping
-- functions using a library function 'xmap': the 'toContinue'
-- part takes the three integers, and produces a 'GameState';
-- whereas 'fromContinue' moves from a suitable 'GameState'
-- back to three integers and rejects all other forms of 'GameState'.

-- We define a similar mapping function for the 'Done' case:

liftDone :: UrlParser r (Int :- r) -> UrlParser r (GameState :- r)
liftDone = xmap toDone fromDone
  where
    toDone   (n :- r)      = Done n :- r
    fromDone (Done n :- r) = Just (n :- r)
    fromDone _             = Nothing

-- We map the empty URL to the state asking the initial question
-- as defined by 'initialQuestion' in Guess.hs. Note that
-- 'fromStart' here always returns 'Nothing', meaning that we
-- never render a game state as the string "/".

liftStart :: UrlParser r r -> UrlParser r (GameState :- r)
liftStart = xmap toStart fromStart
  where
    toStart   r = Continue initialQuestion :- r
    fromStart _ = Nothing

-- The above 'lift' functions may look complicated, but one can
-- actually abstract from them and write them in an easier way.
-- Even better, one can nearly automatically generate them using
-- Template Haskell or so-called datatype-generic programming
-- techniques, which we don't discuss here to keep the tutorial
-- simple.

--------------------------------------------------------------------------
-- Routing and resources:

-- By moving to a typed representation of URLs, we can now implement
-- routing in an entirely typed way. We simply have to provide a
-- mapping from game states to Html pages:

route :: Solver -> GameState -> Html
route _      (Done n)     = renderDonePage n
route solver (Continue s) = renderContinuePage solver s

-- For HTML, we actually use another dedicated datatype 'Html'. We
-- don't plug strings together, but use a library that assists us in
-- providing well-formed 'Html'. For example, the function 'p' below
-- is provided by that library and encodes a single paragraph.
--
-- Since we want to embed strings into HTML, we explicitly convert
-- from strings to HTML using 'stringToHtml'. This function takes care
-- of properly escaping characters that need to be escaped.  Using the
-- 'Html' type thus ensures that we are performing escaping.

renderDonePage :: Int -> Html
renderDonePage n =
    p (stringToHtml $ "The number is " ++ show n ++ "!")

-- The "done" page is very simple.
--
-- The "continue" page is more interesting. We want to not only
-- ask the question to the user, but also provide the possible answers
-- in the form of links. Here, we actually make use of the Solver
-- we have been passing around. We explore all three possible responses
-- and call the solver for each of them, transform the resulting 'GameState'
-- into a URL and create the anchor for the URL in Html.
--
-- Note that because we are using a typed URL (and if we only ever make
-- url link strings using 'toURL') then we can guarantee that we never
-- have any dead links. This is great for refactoring safely.

renderContinuePage :: Solver -> Question -> Html
renderContinuePage solver q@(Q _ g) =
    p (stringToHtml $ "Is " ++ show g ++ " your number?")
      +++
    map p moves
  where
    moves = [ let url = toURL (solver q r)
              in  (anchor ! [href url]) (stringToHtml msg)
            | (msg, r) <- possibleResponses ]

    possibleResponses =
      [("Yes, this is correct",     EQ),
       ("No, my number is smaller", LT),
       ("No, my number is larger",  GT)]

--------------------------------------------------------------------------
-- Adapting to the proper solver:

-- One you've implemented 'guess' in Guess.hs, replace the dumb
-- solver with 'guess' in function 'main' in order to see it in
-- action.

--------------------------------------------------------------------------
-- Defining a derived routing combinator:

-- QuickCheck that was introduced in Guess.hs is an example of an
-- embedded domain-specific language (EDSL). The UrlParser library that
-- we used to define 'stateParser' is another EDSL. An advantage
-- of having such embedded sub-languages is that we can use the
-- abstraction power of Haskell to define new useful operators.
--
-- For example, if we look at 'stateParser', we see many occurrences
-- of the pattern
--
-- ... <.> lit "/" <.> ...
--
-- Define an operator

(</>) :: UrlParser b c -> UrlParser a b -> UrlParser a c
(</>) = error "TODO: implement (</>)"

-- that abstracts from this pattern. Then put the new operator to use
-- by modifying the definition of 'stateParser'.

--------------------------------------------------------------------------
-- Allow restart:

-- In each 'Continue' state, add a fourth link that allows to restart
-- the game from the beginning.

--------------------------------------------------------------------------
--Improving the web interface:
--
-- SPOILER ALERT! You should do the QuickCheck-exercise from Guess.hs
-- before dealing with the following task.
--
-- ... scroll down ...
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
-- ... scroll down ...
--
--
--
--
--
--
-- Ok, so from the failed test we learn that we have situations in
-- which ranges can become empty or even negative. It turns out that
-- this happens in practice if the interactive user is providing
-- inconsistent answers. Try this in the web interface and see if you
-- can get into such a state.
--
-- The proper solution on the game side is to introduce a third
-- constructor to the 'GameState' datatype, representing such a failed
-- state.
--
-- There are at least two possible solutions for the web interface:
--
-- (1) We can map the new constructor for 'GameState' to a page
-- which points out the inconsistency and calls the user a liar.
--
-- (2) We can prevent the user from lying by detecting answers that
-- lead to inconsistent states in advance and not even presenting
-- the links in these cases.
--
-- Implement at least one of these approaches.

--------------------------------------------------------------------------
-- Changing the routing:

-- The goal of this task is to add a new layer of routing. Currently,
-- moving to
--
-- http://localhost:8080/
--
-- starts a new game. We would like to have a welcome page instead
-- that presents an explicit link to start a new game (and could
-- contain other links to do other stuff). Do this by adapting the
-- datatype

data Sitemap = Welcome
             | GuessGame GameState
  deriving Show

-- and using 'Sitemap' rather than 'GameState' as the main datatype
-- for routing. Reuse the old 'route' function and the old
-- 'stateParser', but define new functions around it, and rewire
-- the call to 'route' in 'server' in the end.

--------------------------------------------------------------------------
-- A more classical web serving example:

-- Have a look at GrepWeb.hs which implements another simple web
-- server that actually performs file serving.
