module PositiveInteger where

import Text.Trifecta
import Control.Applicative
import Data.Char

-- 24.11, Ex. 2

parseDigit :: Parser Char
parseDigit = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

{-
peeking at the implementation of `digit`, looks like my implementation could
have been significantly simpler:

digit :: (Stream s m Char) => ParsecT s u m Char
digit               = satisfy isDigit       <?> "digit"
-}

base10Integer :: Parser Integer
base10Integer = do
  digitChars <- some parseDigit
  return $ sum $ zipWith (\a b -> toInteger $ digitToInt a * b) (reverse digitChars) powersOfTen

-- my initial attempt
powersOfTen' :: [Int]
powersOfTen' = map (\a -> round $ 10 ^^ a) [0..]

-- improved thanks for #haskell-beginners
powersOfTen :: [Int]
powersOfTen = iterate (10 *) 1

-- Ex. 3, like Ex.2 but handle negative integers
base10Integer' :: Parser Integer
base10Integer' = base10Integer <|> parseNegativeBase10Integer

parseNegativeBase10Integer :: Parser Integer
parseNegativeBase10Integer = char '-' *> ((*(-1)) <$> base10Integer)
