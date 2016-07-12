{-# OPTIONS_GHC -Wall #-}

module AsPatterns where
  
import Data.Char
  
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf sub s = and $ map ((flip elem) s) sub

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\wd@(x:xs) -> (wd, (toUpper x):xs)) $ words s

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x):xs

splitSentences :: String -> [String]
splitSentences s =
  let 
    positions = foldr (\(c,n) acc -> if c == '.' then n:acc else acc) [] $ zip s [0..]
    (orig', splits') = foldr (\n (orig, splits) -> let (first, second) = splitAt n orig in (first, second:splits)) (s, []) positions
  in 
    (orig':splits')
  
capitalizeSentence :: String -> String
capitalizeSentence [] = []
capitalizeSentence xs@(".") = xs
capitalizeSentence ('.':' ':x:xs) =  ". " ++ (toUpper x):xs
capitalizeSentence ('.':x:xs) =  '.':(toUpper x):xs
capitalizeSentence (x:xs) =  (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph s = concat $ map capitalizeSentence $ splitSentences s