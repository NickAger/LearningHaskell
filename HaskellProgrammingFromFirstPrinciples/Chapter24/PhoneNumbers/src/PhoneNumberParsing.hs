module PhoneNumberParsing where

import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

{- needs to be able to parse:

123-456-7890
1234567890
(123) 456-7890
1-123-456-7890

-}
data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = parseBracketAndDash <|> parseWithCountryCode <|> try parseSeparatedByDashes <|> parseNoSeparators

parseWithCountryCode :: Parser PhoneNumber
parseWithCountryCode = string "1-" *> parseSeparatedByDashes

parseSeparatedByDashes :: Parser PhoneNumber
parseSeparatedByDashes = PhoneNumber <$> parseInt <*> (char '-' *> parseInt) <*> (char '-' *> parseInt)

parseInt :: Parser Int
parseInt = fromInteger <$> integer

-- 1234567890
parseNoSeparators :: Parser PhoneNumber
parseNoSeparators = liftA3 PhoneNumber (parseDigits 3) (parseDigits 3) (parseDigits 4)
  where
    parseDigits :: Int -> Parser Int
    parseDigits n = read <$> count n digit

-- (123) 456-7890
parseBracketedAreaCode :: Parser Int
parseBracketedAreaCode = char '(' *> parseInt <* char ')'

parseBracketAndDash :: Parser PhoneNumber
parseBracketAndDash = liftA3 PhoneNumber parseBracketedAreaCode (whiteSpace *> parseInt) (char '-' *> parseInt)
