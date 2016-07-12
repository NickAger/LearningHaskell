module Main where

--------------------------------------------------------------------------
-- Introduction:

-- In this module, we implement a simple spell checker.

-- Let's starts simple:
--
-- For the beginning, let's assume our dictionary is a list of strings:

type Dictionary = [String]

-- If you don't know them yet, try to familiarize yourself with the
-- functions
--
--   words
--   lines

-- Now write a function

spellCheck :: String -> Dictionary -> [String]
spellCheck = error "TODO: implement spellCheck"

-- that takes a text given as a string, a dictionary, and lists all the
-- words from the text that do not appear in the dictionary.

-- Now let's write an IO wrapper:

spellCheckFiles :: FilePath -> FilePath -> IO ()
spellCheckFiles inputFile dictionaryFile = error "TODO: implement spellCheckFiles"

-- Read the files from disk. It's ok for now to crash if the files are
-- missing. Print the list of words to the screen, line by line.

-- In order to test the function, try to download a text-based dictionary
-- from the internet (there are many files online that have one word per
-- line). On Unix systems, there are often suitable dictionaries in
-- the /usr/share/dict directory.

--------------------------------------------------------------------------
-- Main program:

-- Try to write a proper main program that takes the dictionary and file
-- to check from the command line.

--------------------------------------------------------------------------
-- Efficiency:

-- If you have the program running, make a backup copy (or use a version
-- control system), and then try to switch to a version based on
--
-- type Dictionary = Set String
--
-- instead.

--------------------------------------------------------------------------
-- Further improvements:

-- * Adapt the spell checker to do the comparison in a case-insensitive
-- manner. However, incorrect words should still be printed in the way
-- they appeared in the original text.
--
-- * Adapt the spell checker to print a line and column number for every
-- word that is found to be incorrect.
--
-- * Make further improvements you can think of.
