module Chapter9 where
  
import Data.Bool
import Data.Char
import Data.List

myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo f t 
  | f > t = []
  | otherwise = f : (myEnumFromTo (succ f) t)
      

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool f t 
  | f == t = [f]
  | otherwise = []


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd f t
  | f == t = [f]
  | otherwise = []
  
eftInt :: Int -> Int -> [Int]
eftInt f t 
  | f > t = []
  | otherwise = f : eftInt (succ f) t


eftChar :: Char -> Char -> [Char]
eftChar f t 
  | f > t = []
  | otherwise = f : eftChar (succ f) t


myWords :: String -> [String]
myWords [] = []
myWords s = (extractWord s) : myWords (removeWord s) 
  where
    extractWord s =
      takeWhile (/=' ') s
      
    removeWord s =
       dropWhile (==' ') $ dropWhile (/=' ') s  


--- 

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen




myLines :: String -> [String]
myLines [] = []
myLines s = (extractLine s) : myLines (removeLine s)
  where
    extractLine s =
      takeWhile (/='\n') s
      
    removeLine s =
       dropWhile (=='\n') $ dropWhile (/='\n') s
-- What we want 'myLines sentences' to equal

shouldEqual = 
  [ 
  "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"   ]
       -- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)


split :: Char -> String -> [String]
split _ [] = []
split c s = (extractTo c s) : split c (removeTo c s)
  where
    extractTo c s =
      takeWhile (/=c) s
      
    removeTo c s =
       dropWhile (==c) $ dropWhile (/=c) s
       
myLines2 :: String -> [String]
myLines2 s = split '\n' s

myWords2 :: String -> [String]
myWords2 s = split ' ' s

mySqr = [ x^2 | x <- [1..5]]
myCube = [ y^3 | y <- [1..5]]

blah = enumFromTo 'a' 'z'


itIsMystery xs = map (\x -> elem x "aeiou") xs

myPower = (^)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs)  = (a, b) : myZip as bs


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs)  = (f a b) : myZipWith f as bs

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 a b = myZipWith (,) a b

capitalise :: String -> String
capitalise (a:b) = (toUpper a) : b

capitalise2 :: String -> String
capitalise2 [] = []
capitalise2 (a:b) = (toUpper a) : capitalise2 b

capitalise3 :: String -> Char
capitalise3 (a:_) = toUpper a

capitalise4 :: String -> Char
capitalise4 a = toUpper $ head a

capitalise5 :: String -> Char
capitalise5 a = toUpper . head $ a

capitalise6 :: String -> Char
capitalise6 = toUpper . head 

-- see also Ciphers.

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a xs = myAny (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
--myMaximumBy comp (x:xs) = go x xs
--  where
--    go maxSoFar (x:[]) = max maxSoFar x
--    go maxSoFar (x:xs) = go (max maxSoFar x) xs
--    max x1 x2 = if (comp x1 x2) == LT then x2 else x1

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = go x xs
  where
    go maxSoFar [] = maxSoFar
    go maxSoFar (x:xs) = go (max maxSoFar x) xs
    max x1 x2 = if (comp x1 x2) == LT then x2 else x1
    
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = go x xs
  where
    go minSoFar [] = minSoFar
    go minSoFar (x:xs) = go (min minSoFar x) xs
    min x1 x2 = if (comp x1 x2) == LT then x1 else x2

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
