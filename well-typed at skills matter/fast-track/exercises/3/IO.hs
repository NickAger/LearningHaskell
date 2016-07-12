module IO where

import Control.Monad hiding (filterM, mapM, sequence)
import Data.Char
import Data.Functor
import Data.List
import Data.IORef
import Data.Time
import System.Directory
import System.Environment
import System.Random
import System.IO
import Prelude hiding (mapM, sequence)

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the part on IO in Haskell.
--
-- The central examples are included.

--------------------------------------------------------------------------
-- Examples (The IO type):

-- Try in GHCi:
--
-- :t getLine
-- :t putStrLn
-- :t putStrLn "Hello"
--
-- Note that GHCi treats term of IO type *differently* from other
-- type when you type them in at the prompt.
--
-- An IO action is first executed. Effects of the execution are
-- visible. Finally, the result of the IO action is printed, except
-- if the IO action had type 'IO ()'.
--
-- Therefore, typing
--
-- getLine
--
-- will first perform the action (read the line which you have to
-- type in), then show the result, i.e., print the string back.
--
-- And typing
--
-- putStrLn "Hello"
--
-- will print the string (note the missing quotes, this is the
-- printed string, not a result of 'String' type), and then nothing
-- more.

--------------------------------------------------------------------------
-- IO actions are first-class citizens:

-- Try in GHCi:
--
-- :t fst (length [getLine, getLine], putStrLn "Hello")
-- fst (length [getLine, getLine], putStrLn "Hello")
--
-- Note that the type doesn't mention IO. In general: if it doesn't
-- have IO type, it cannot do IO.
--
-- It holds even more generally: if the type doesn't mention IO on
-- the outside, it won't do IO. For example, a list of IO actions
-- is a list, nothing more, nothing less.

--------------------------------------------------------------------------
-- Composing IO actions:

-- Try in GHCi:
--
-- putStr "Hello " >> putStr "world\n"
-- putStr "Hello world\n"
-- putStr "Hello world"
-- putStrLn "Hello world"

--------------------------------------------------------------------------
-- How to (not) use results of IO actions:

-- Verify that these yield a type error:
--
-- map toUpper getLine
-- putStrLn getLine

--------------------------------------------------------------------------
-- The "bind" operator:

echo :: IO ()
echo = getLine >>= putStrLn

-- Note the subtle difference in GHCi between 'echo' and just 'getLine'.
-- While 'getLine' produces a 'String' result which is subsequently printed
-- (with quotes), the 'echo' action prints the string back (without quotes)
-- and has a '()' result (which GHCi doesn't print).

--------------------------------------------------------------------------
-- Examples:

echoTwice :: IO ()
echoTwice = getLine >>= \ xs -> putStrLn xs >> putStrLn xs

greeting :: IO ()
greeting =
  putStrLn "Who are you?"  >>
  getLine                  >>= \ name ->
  putStrLn ("Hello, " ++ name ++ "!")

-- This is still a type error:

-- capitalize' = getLine >>= \ xs -> map toUpper xs -- type error

--------------------------------------------------------------------------
-- Examples:

capitalize :: IO String
capitalize = getLine >>= \ xs -> return (map toUpper xs)

dieRoll :: IO Int
dieRoll = randomRIO (1, 6)

rollTwice :: IO (Int, Int)
rollTwice = dieRoll >>= \ x -> dieRoll >>= \ y -> return (x, y)

--------------------------------------------------------------------------
-- The IO type is a functor:

capitalize' :: IO String
capitalize' = fmap (map toUpper) getLine

-- The (<$>) operator is a different name for 'fmap':

capitalize'' :: IO String
capitalize'' = map toUpper <$> getLine

--------------------------------------------------------------------------
-- A simple example -- overly IO-centric:

count :: Int -> IO ()
count n = go 0
  where
    go :: Int -> IO ()
    go i  | i > n      =  return ()
          | even i     =  print i >> go (i + 1)
          | otherwise  =  go (i + 1)

--------------------------------------------------------------------------
-- A simple example -- keeping IO separated:

evens :: Int -> [Int]
evens n = filter even [0 .. n]

arrangeInLines :: Show a => [a] -> String
arrangeInLines = unlines . map show

count' :: Int -> IO ()
count' = putStr . arrangeInLines . evens

--------------------------------------------------------------------------
-- Copying the first line of a file:

copyFileLine :: FilePath -> FilePath -> IO ()
copyFileLine src tgt =
  withFile src  ReadMode   $  \ srcH  ->
  withFile tgt  WriteMode  $  \ tgtH  ->
    hGetLine srcH >>= hPutStrLn tgtH

-- It'll be an exercise to write a better file copying function.

--------------------------------------------------------------------------
-- do notation:

example :: IO ()
example = do
  hSetBuffering stdout NoBuffering
  putStr "What is your name? "
  name <- getLine
  putStr "Where do you live? "
  loc <- getLine
  let answer  | loc == "London"  =    "That's wonderful!"
              | otherwise        =    "Sorry, " ++ name
                                 ++   ", where is " ++ loc ++ "?"
  putStrLn answer

--------------------------------------------------------------------------
-- Nested do:

loop :: IO ()
loop = do
  putStrLn "Type 'q' to quit."
  c <- getChar -- reads a single character
  if c == 'q'
    then putStrLn "Goodbye"
    else do
      putStrLn "Here we go again ..."
      loop

--------------------------------------------------------------------------
-- About return:

justTwo = do
  n <- return 2
  print n

-- Also try executing this:

justTwo' = do
  return 3
  print 2

--------------------------------------------------------------------------
-- Mutable variables (module Data.IORef):

testIORefs = do
  var1 <- newIORef 3
  var2 <- newIORef 7
  modifyIORef var1 (+8)
  val1 <- readIORef var1
  modifyIORef var2 (\ x -> x - val1)
  readIORef var2

--------------------------------------------------------------------------
-- Random numbers (module System.Random):

-- Try in GHCi:
--
-- randomIO :: IO Bool
-- randomIO :: IO Int
-- randomIO :: IO Float
-- randomIO :: IO Char

--------------------------------------------------------------------------
-- Access system properties (Data.Time, System.Environment):

-- Try in GHCi:
--
-- getCurrentTime
-- getProgName
-- getArgs
-- getEnvironment

--------------------------------------------------------------------------
-- Accessing the file system (System.Directory):

-- Try in GHCi:
--
-- getHomeDirectory
-- getCurrentDirectory
-- getDirectoryContents "."
-- doesFileExist "."
-- doesDirectoryExist "."

--------------------------------------------------------------------------
-- Defining 'filterM':

filterM :: (a -> IO Bool) -> [a] -> IO [a]
filterM p []        =  return []
filterM p (x : xs)  =  do
  bool          <-  p x
  filteredTail  <-  filterM p xs
  return (if bool then x : filteredTail else filteredTail)

--------------------------------------------------------------------------
-- Listing files recursively:

recursiveFiles :: FilePath -> IO [FilePath]
recursiveFiles dir = do
  contents  <-  getDirectoryContents dir
  subdirs   <-  filterM doesDirectoryExist contents
  let subdirs' = filter (not . ("." `isPrefixOf`)) subdirs
  recs      <-  mapM recursiveFiles subdirs'
  return (contents ++ concat recs)

--------------------------------------------------------------------------
-- Defining 'mapM':

sequence :: [IO a] -> IO [a]
sequence []        =  return []
sequence (x : xs)  =  do
  r   <-  x
  rs  <-  sequence xs
  return (r : rs)

mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f = sequence . map f

--------------------------------------------------------------------------
-- Using 'sequence':

-- Try to predict what will happen, then execute in GHCi:
--
-- sequence [randomRIO (1,10), return 5, fmap read getLine]
