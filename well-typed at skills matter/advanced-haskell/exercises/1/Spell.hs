-- Spell.hs
-- Copyright (c) 2013 Well-Typed LLP

import System.Environment
import Data.Char

type DictionaryFile = FilePath
type TextFile       = FilePath
type Dictionary     = [String]

main :: IO ()
main =
  do
    -- We expect two command-line arguments:
    --
    --   the dictionary file
    --   the text file
    --
    -- We are going to crash with an unhelpful error message
    -- if these are not provided as intended.
    [dictf,txtf] <- getArgs
    ws <- spellIO dictf txtf
    putStrLn $ unlines ws

-- | The function 'spellIO' is the actual program. It takes
-- two pathnames, one for dictionary, one for text, reads
-- the two files, performs spell checking, and returns the
-- list of unknown words.
spellIO :: DictionaryFile -> TextFile -> IO [String]
spellIO dictf txtf =
  do
    -- build the dictionary
    list <- readFile dictf
    let dict = map simplify (lines list) -- simplify is defined below
    -- read and process the text file
    txt <- readFile txtf
    return (spell dict txt)

-- | The pure spell checking function. Individual words are simplified
-- on the fly so that we report the incorrect word in its original
-- form, as it appears in the text.
spell :: Dictionary -> String -> [String]
spell dict txt = filter (\ w -> not (simplify w `elem` dict)) allWords
  where
    allWords = concatMap words (lines txt)

-- | The function simplify strips all non-alphabetic characters from
-- a word and turns it into lowercase form. This is a simplistic approach
-- so that we can deal with punctuation and capitalization.
simplify :: String -> String
simplify = map toLower . filter isAlpha
