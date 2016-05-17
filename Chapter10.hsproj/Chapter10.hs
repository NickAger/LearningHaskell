module Chapter10 where
  
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
                  
theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime 
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime 
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]
                 
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate date) dates = date : dates
    f _ dates = dates
    
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber x) xs = x : xs
    f _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr max (head dates) dates
  where
    dates = filterDbDate items
    
sumDb :: [DatabaseItem] -> Integer
sumDb items = sum (filterDbNumber items)

avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral (sum numbers)) / (fromIntegral (length numbers))
  where
    numbers = filterDbNumber items

fibs1 = take 20 go
  where
     go = 1 : scanl (+) 1 go
     
fibs2 = takeWhile (<100) go
  where
     go = 1 : scanl (+) 1 go
     
stops = "pbtdkg"
vowels = "aeiou"

stopVowelsStops = [(a,b,c) | a <- stops, b <- vowels, c <- stops]

stopVowelsStops2 = filter (\(a,_,_) -> a == 'p') stopVowelsStops

verbs = ["act", "believe", "check", "debate", "encourage"]
nouns = ["ball", "mouse", "house", "garden", "car"]

nounVerbNouns = [(a,b,c) | a <- nouns, b <- verbs, c <- nouns]

seekritFunction x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x a -> a || (f x))  False

myElem :: Eq a => a -> [a] -> Bool
myElem x  = foldr (\x' a -> a || (x' == x)) False 

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = any (==x) 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x a -> (f x) : a) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr f' [] 
  where
    f' x a
      | (f x) = x : a
      | otherwise = a  
 
squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x a -> (f x) ++ a) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1 max
  where
    max x1 x2 = if (f x1 x2) == GT then x1 else x2
    
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 min
  where
    min x1 x2 = if (f x1 x2) == LT then x1 else x2
    

