{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Control.Applicative
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
import Text.Trifecta

type Hours = Int
type Minutes = Int
type Description = String
type Year = Int
type Month = Int
type Day = Int

data Time = Time Hours Minutes deriving (Eq, Show)
data LogEntry = LogEntry Time Description deriving (Eq, Show)
data Date = Date Year Month Day deriving (Eq, Show)
data LoggedDay = LoggedDay Date [LogEntry] deriving (Eq, Show)
data LogFile = LogFile [LoggedDay] deriving (Eq, Show)

logSample = [r|
-- comment

# 2025-02-05

08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

parseComment :: Parser ()
parseComment = string "--" *> manyTill anyChar newline *> pure ()

{-
2016-05-03 16:52:50

chipf0rk>	I'm trying to parse everything before it
<chipf0rk>	and that's the part that's tripping me up;
<nitrix>	Then you can do manipulations like skipMany comments, or, optional comment, or sepBy notComment comment, etc.
<nitrix>	Where notComment would be = manyTill anyChar (lookAhead (comment <|> eof))

-}



{--
skipComment :: Parser ()
skipComment = skipOptional (string "--" >> manyTill anyChar (void newline <|> eof))

skipSpace :: Parser ()
skipSpace = void $ (char ' ') <|> tab

skipEmpty :: Parser ()
skipEmpty = void newline <|> skipSpace

--}

parseStartTime :: Parser Time
parseStartTime = liftA2 Time (parseInt <* char ':')  (parseInt <* space)

parseInt :: Parser Int
parseInt = fromInteger <$> integer
