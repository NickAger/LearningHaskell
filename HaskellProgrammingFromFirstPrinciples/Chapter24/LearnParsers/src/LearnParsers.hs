module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

-- 1.
-- eof :: m ()
-- one :: Parser Char
oneEof = one >> eof
oneTwoEof = oneTwo >> eof

-- 2.
all2 :: Parser String
all2 = string "123"
all2' = string "123" >> (eof :: Parser ())
