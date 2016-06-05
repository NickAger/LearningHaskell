{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module VigenereCipher where
  
import Data.Char
 
-- I was attached to using cycle and zip to provide the seed letters
-- for each letter in the target phrase. However that then meant
-- that I had to remove the spaces, remembering their location
-- then re-insert them - I'm sure there is a better way, but it works...
vigenèreCipher :: String -> String -> String
vigenèreCipher seed s = 
  let (filteredTarget, spaces) = filterSpaces s
      cipher = applyCipher seed filteredTarget
  in insertSpaces spaces cipher
  where
    applyCipher :: String -> String -> String
    applyCipher seed s = map applyCipherToChar $ letterCiphers seed s
    
    applyCipherToChar :: (Char, Int) -> Char
    applyCipherToChar (c, s) = chr ((ord 'A') + ((ord . toUpper $ c) - (ord 'A') + s) `mod` 26)
    
    letterCiphers :: String -> String -> [(Char, Int)]
    letterCiphers seed s = zip s (cycle $ map (\c -> ((ord.toUpper) c) - (ord 'A')) seed)
    
    filterSpaces :: String -> (String, [Int])
    filterSpaces s = foldr (\(c, n) (s, spaces) -> if c == ' ' then (s, n:spaces) else (c:s ,spaces)) ([], []) $ zip s [0..]
    
    insertSpaces :: [Int] -> String -> String
    insertSpaces spaces s = foldr (\n acc -> let (fst, snd) = splitAt n acc in fst ++ (' ':snd)) s $ reverse spaces 
    
