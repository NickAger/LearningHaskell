-- working through:
-- http://slpopejoy.github.io/posts/2015-04-10-Types.html

import Data.Set (Set, fromList)
import Data.Map (Map, insertWith, empty, split, elems)
import qualified Data.Map as M

duplicate :: Int -> String -> String
duplicate count s = concat (replicate count s)

data Person = Person { name :: String, age :: Int } deriving Show

agesPointFree :: [Person] -> Set Int
agesPointFree = fromList . map age

indexPersonsByName :: [Person] -> Map String [Person]
indexPersonsByName = foldl ins empty 
    where ins m p = insertWith (++) (name p) [p] m
    
type Name = String
type Age = Int

type PersonsByAge = Map Age [Person]
type PersonsByName = Map Name [Person]


indexPersonsByAge :: [Person] -> PersonsByAge
indexPersonsByAge = foldl ins empty 
    where ins m p = insertWith (++) (age p) [p] m

getPersonsOver :: Age -> PersonsByAge -> [Person]
getPersonsOver a = concat . elems . snd . split a

-- partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
--  partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- split :: Ord k => k -> Map k a -> (Map k a, Map k a)
--  split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])