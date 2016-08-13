{-# LANGUAGE QuasiQuotes #-}

module LogFileParser where

import Control.Applicative
import Data.Functor
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.LookAhead

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

logSample :: String
logSample = [r|

-- wheee a comment

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

{-
2016-05-03 16:52:50

chipf0rk>	I'm trying to parse everything before a comment
<chipf0rk>	and that's the part that's tripping me up;
<nitrix>	TYou can do manipulations like skipMany comments, or, optional comment, or sepBy notComment comment, etc.
<nitrix>	Where notComment would be = manyTill anyChar (lookAhead (comment <|> eof))
-}

parseStartTime :: Parser Time
parseStartTime = liftA2 Time (parseInt <* char ':') parseInt

parseInt :: Parser Int
parseInt = fromInteger <$> integer

-- to avoid `integer` eating trailing whitespace including newlines
parseInt' :: Parser Int
parseInt' = read <$> some digit

parseLogEntry :: Parser LogEntry
parseLogEntry = liftA2 LogEntry parseStartTime parseDescriptionIgnoringComment

parseDescriptionIgnoringComment :: Parser Description
parseDescriptionIgnoringComment = try parseLineWithComment <|> parseToNextLineOrEof

parseLineWithComment :: Parser String
parseLineWithComment = manyTill anyChar (string "--") <* parseToNextLineOrEof

parseToNextLineOrEof :: Parser String
parseToNextLineOrEof = manyTill anyChar (void newline <|> eof)

parseToNextLine :: Parser String
parseToNextLine = manyTill anyChar newline

parseComment :: Parser ()
parseComment = string "--" *> manyTill anyChar newline *> pure ()

-- # 2025-02-05
parseDayLine :: Parser Date
parseDayLine = char '#' *> space *> liftA3 Date (parseInt <* char '-')  (parseInt <* char '-') (parseInt' <* parseToNextLine)

parseLoggedDay :: Parser LoggedDay
parseLoggedDay = liftA2 LoggedDay parseDayLine parseLogEntries

parseLogEntries :: Parser [LogEntry]
parseLogEntries = many parseLogEntry

parseLogFile :: Parser [LoggedDay]
parseLogFile = many (parseUpToLoggedDay *> parseLoggedDay)

parseUpToLoggedDay :: Parser Char
parseUpToLoggedDay = manyTill anyChar (lookAhead $ string "\n#") *> newline

--

testParser :: Result [LoggedDay]
testParser = parseString parseLogFile mempty logSample

{-
testParser gives:

Success [LoggedDay (Date 2025 2 5) [LogEntry (Time 8 0) "Breakfast\n09:00 Sanitizing moisture collector\n11:00 Exercising in high-grav gym\n12:00 Lunch\n13:00 Prog
ramming\n17:00 Commuting home in rover\n17:30 R&R\n19:00 Dinner\n21:00 Shower\n21:15 Read\n22:00 Sleep\n\n# 2025-02-07 ",LogEntry (Time 8 0) "Breakfast ",LogEntry
(Time 9 0) "Bumped head, passed out",LogEntry (Time 13 36) "Wake up, headache",LogEntry (Time 13 37) "Go to medbay",LogEntry (Time 13 40) "Patch self up",LogEntry
(Time 13 45) "Commute home for rest",LogEntry (Time 14 15) "Read",LogEntry (Time 21 0) "Dinner",LogEntry (Time 21 15) "Read",LogEntry (Time 22 0) "Sleep"]]

I don't understand why its parsing the first log entry as the whole of the first day.
-}
