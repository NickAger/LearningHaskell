module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p =
    print $ parseString p mempty "123"

testParseBottom :: Parser () -> IO ()
testParseBottom p =
    print $ parseString p mempty "123"

-- 1.
-- eof :: m ()
-- one :: Parser Char
oneEof = one >> eof
oneTwoEof = oneTwo >> eof

-- 2.
all2 :: Parser String
all2 = string "123"

all2' :: Parser ()
all2' = string "123" >> eof

all2'' :: Parser String
all2'' = string "123" >>= (\rst -> eof >> return rst)

all2''' :: Parser String
all2''' = string "123" <* eof

-- trick is to try to parse the largest first
any2'' :: Parser String
any2'' = stringEof "123" <|> stringEof "12" <|> stringEof "1"
  where
    stringEof s =
      string s >>= (\rst -> eof >> return s)

any2''' :: Parser String
any2''' = (string "123" <|> string "12" <|> string "1") >> stop

any2'''' :: Parser String
any2'''' = stringStop "123" <|> stringStop "12" <|> stringStop "1"
  where
    stringStop s =
      string s >> stop

any2''''' :: Parser String
any2''''' = stringEof "123" <|> stringEof "12" <|> stringEof "1"
  where
    stringEof s =
      string s <* eof

-- 3.
string' :: String -> Parser String
string' = mapM char
