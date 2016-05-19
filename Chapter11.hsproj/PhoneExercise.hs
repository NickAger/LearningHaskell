module PhoneExercise where
import Data.Char
import Data.List
import GHC.Exts
  
-- validButtons = "1234567890*#"
type Digit = Char

data PhoneButton = PhoneButton { digit :: Digit    , chars :: [Char] }
    deriving (Eq, Show)

data DaPhone = DaPhone {buttons :: [PhoneButton]}
  deriving Show


standardKeypad = DaPhone [PhoneButton '1' "1", PhoneButton '2' "abc2", PhoneButton '3' "def3", PhoneButton '4' "ghi4", PhoneButton '5' "jkl5", PhoneButton '6' "mno6", PhoneButton '7' "pqrs7", PhoneButton '8' "tuv8", PhoneButton '9' "wxyz9", PhoneButton '*' "^*", PhoneButton '0' "+ 0", PhoneButton '#' ".,#" ]

-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps keypad c = 
  let
    lc = toLower c
    button = (find (\b -> lc `elem` (chars b)) $ buttons keypad)
    index = chars <$> button >>= elemIndex lc
    result = keypress button index
  in
    if isUpper c then ('*', 1):result else result
  where
    keypress :: Maybe PhoneButton -> Maybe Int -> [(Digit, Presses)]
    keypress Nothing Nothing = []
    keypress _ Nothing = []
    keypress Nothing _ = []
    keypress (Just b) (Just i) = [(digit b, i + 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead keypad string = concat $ map (reverseTaps keypad) string

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps keys = sum $ map snd keys

mostPopularLetter :: String -> Char
mostPopularLetter s = 
  let 
    buttonFreqs = buttonFrequency s
    mostPopButton = maxButtonFreq buttonFreqs
  in
    fst mostPopButton 
  where
      buttonFrequency :: String -> [(Digit, Presses)]
      buttonFrequency s = 
        let
          groups = groupWith fst $ cellPhonesDead standardKeypad s
        in
          map (\presses -> (fst (head presses) ,fingerTaps presses)) groups
          
      maxButtonFreq :: [(Digit, Presses)] -> (Digit, Presses)
      maxButtonFreq buttonFreqs = foldr1 chooseMostPressed buttonFreqs
        where       
          chooseMostPressed :: (Digit, Presses) -> (Digit, Presses) -> (Digit, Presses)
          chooseMostPressed a b = if snd a > snd b then a else b

coolestLtr :: [String] -> Char
coolestLtr messages = mostPopularLetter $ concat messages

-- instead I could have made a (length, word) tuple then sorted on the tuple
-- but that sounds less efficient than iterating over with a foldr
coolestWord :: [String] -> String
coolestWord messages = 
  let
    groupedWords = group $ sort $ concat $ map words messages
    biggestGroup = foldr1 chooseBiggestGroup groupedWords
  in
    head biggestGroup
  where
    chooseBiggestGroup :: [String] -> [String] -> [String]
    chooseBiggestGroup a b = if length a > length b then a else b
    

