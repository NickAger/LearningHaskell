{-# LANGUAGE QuasiQuotes #-}

{-
Exercise 5.

Write a parser for a log file format and sum the time spent in each
activity. Additionally, provide an alternative aggregation for the data
that provides average time spent per activity per day.
-}

module LogFileParser where

import Control.Applicative
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.LookAhead

type Hours = Int
type Minutes = Int
type Activity = String
type Year = Int
type Month = Int
type Day = Int

data Time = Time Hours Minutes deriving (Eq, Show)
data LogEntry = LogEntry Time Activity deriving (Eq, Show)
data Date = Date Year Month Day deriving (Eq, Show)
data LoggedDay = LoggedDay Date [LogEntry] deriving (Eq, Show)
data LogFile = LogFile [LoggedDay] deriving (Eq, Show)

type LogFileString = String
logSample :: LogFileString
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


parseStartTime :: Parser Time
parseStartTime = liftA2 Time (parseInt <* char ':') parseInt

parseInt :: Parser Int
parseInt = fromInteger <$> integer

-- to avoid `integer` eating trailing whitespace including newlines
parseInt' :: Parser Int
parseInt' = read <$> some digit

parseLogEntry :: Parser LogEntry
parseLogEntry = liftA2 LogEntry parseStartTime parseActivityIgnoringComment

parseActivityIgnoringComment :: Parser Activity
parseActivityIgnoringComment = try parseLineWithComment <|> parseToNextLineOrEof

parseLineWithComment :: Parser String
parseLineWithComment = manyTill (noneOf "\n") (many (char ' ') *> string "--") <* parseToNextLineOrEof

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
type DurationMinutes = Int

duration :: Time -> Time -> DurationMinutes
duration (Time startHour startMinute) (Time endHour endMinute) = ((endHour - startHour) * 60) + endMinute - startMinute

activityDurations :: [LogEntry] -> [(Activity, DurationMinutes)]
activityDurations [] = []
activityDurations entries =
  let
    durations = zipWith zipEntries entries (tail entries)
  in
    durations ++ [lastDuration (last entries)]
  where
    zipEntries (LogEntry start activity) (LogEntry end _) = (activity, duration start end)
    lastDuration (LogEntry start activity) = (activity, duration start (Time 23 59))


collectActivities :: [LoggedDay] -> Map Activity [DurationMinutes]
collectActivities days =
  let
    activities = concatMap (\(LoggedDay _ entries) -> activityDurations entries) days
    in foldr (\(activity, duration) mp -> M.insertWithKey (const (++)) activity [duration] mp) M.empty activities

collectActivitiesLog :: LogFileString -> Result (Map Activity [DurationMinutes])
collectActivitiesLog logFile =
  let
    parserResult = parseString parseLogFile mempty logFile
    in fmap collectActivities parserResult

sumActivitiesLog :: LogFileString -> Result (Map Activity DurationMinutes)
sumActivitiesLog logFile =
  (fmap.fmap) sum $ collectActivitiesLog logFile

averageActivitiesLog :: LogFileString -> Result (Map Activity DurationMinutes)
averageActivitiesLog logFile =
  (fmap.fmap) average $ collectActivitiesLog logFile
    where
      average a = sum a `div` length a
