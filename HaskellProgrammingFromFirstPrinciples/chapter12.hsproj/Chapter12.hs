import Data.Maybe (fromMaybe)
import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s


replaceThe :: String -> String
replaceThe s = unwords $ map (fromMaybe "a".notThe) $ words s


isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

beginsWithVowel :: String -> Bool
beginsWithVowel [] = False
beginsWithVowel (c:_) = isVowel c

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go (0, words s)
  where
    go :: (Int, [String]) -> Int
    go (n,[]) = n
    go (n, _:[]) = n
    go (n, "the":s2:ss) = go (incrementCountIfVowel s2 n, s2:ss)
    go (n, s:ss) = go (n, ss)
   
    incrementCountIfVowel :: String -> Int -> Int
    incrementCountIfVowel s n = if beginsWithVowel s then n+1 else n
    
countTheBeforeVowel2 :: String -> Int
countTheBeforeVowel2 s = 
  let
    wds = words s
    pairs = zip wds $ tail wds
    onlyThePairs = filter ((=="the").fst) pairs
  in 
    length $ filter (beginsWithVowel.snd) onlyThePairs
    
    
countVowels :: String -> Int
countVowels s = foldr incrementCountIfVowel 0 s
  where
    incrementCountIfVowel :: Char -> Int -> Int
    incrementCountIfVowel c n = if isVowel c then n+1 else n
    
newtype Word' =
  Word' String
  deriving (Eq, Show)
  
mkWord :: String -> Maybe Word'
mkWord s = if countVowels s > length s `div` 2 then Nothing else Just (Word' s)

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)
  
natToInteger :: Nat -> Integer
natToInteger nat = go nat 0
  where
      go :: Nat -> Integer -> Integer
      go Zero i = i
      go (Succ nat) i = go nat (i+1)
      
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go Zero i)
  where
     go :: Nat -> Integer -> Nat
     go nat 0 = nat
     go nat i = go (Succ nat) (i-1)
     
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee default' _ Nothing = default'
mayybee _ f (Just v) = f v

fromMaybe' :: a -> Maybe a -> a
fromMaybe' default' Nothing = default'
fromMaybe' _ (Just v) = v

fromMaybe2 :: a -> Maybe a -> a
fromMaybe2 default' v = mayybee default' id v

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just v) = [v]

maybeToList2 :: Maybe a -> [a]
maybeToList2 v = mayybee [] (:[]) v

catMaybes :: [Maybe a] -> [a]
catMaybes ms =
  let
    filtered = filter isJust ms
  in
    map (stripJust) filtered
  where
    stripJust (Just v) = v
    
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = 
  let
    list = catMaybes ms
  in
    if length list < length ms then Nothing else Just list
    

lefts' :: [Either a b] -> [a]
lefts' = foldr accumulateIfLeft [] 
  where
    accumulateIfLeft (Left v) acc = v:acc
    accumulateIfLeft _ acc = acc  
   
rights' :: [Either a b] -> [b]
rights' = foldr accumulateIfRight [] 
  where
    accumulateIfRight (Right v) acc = v:acc
    accumulateIfRight _ acc = acc
    
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr accumulate ([], []) 
  where
    accumulate (Left v) (left, right) = (v:left, right)
    accumulate (Right v) (left, right) = (left, v:right)
    
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right v) = Just (f v)


myIterate :: (a -> a) -> a -> [a]
myIterate f s  = 
  let
    next = f s
  in    
    s:(myIterate f next)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = go (f s)
  where
    go Nothing = []
    go (Just (a,b)) = a:(myUnfoldr f b)
    

betterIterate :: (a -> a) -> a -> [a] 
betterIterate f s = myUnfoldr (\b -> Just(b, f b)) s

  