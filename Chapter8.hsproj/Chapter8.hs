module Chapter8 where
  
import Data.List (intersperse)
  
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)

recursiveMultiply :: Integral a => a -> a -> a
recursiveMultiply n 1 = n
recursiveMultiply n t = n + recursiveMultiply n (t - 1)

data DividedResult a = 
    Result a
  | DividedByZero
  deriving (Show)
  
dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy num denom 
  | denom < 0 = Result (-(fst $ go num (-denom) 0))
  | otherwise = Result (fst $ go num denom 0)
  where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)
            

mc91 :: Integral a => a -> a
mc91 n 
  | n > 100 = n - 10
  | otherwise = mc91(mc91 (n+11))


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

-- works - just a little ugly
--digits :: Int -> [Int]
--digits n = map (\c -> (read [c]) :: Int) (show n)

digits :: Int -> [Int]
digits n = go n []
  where go n acc
          | n < 10 = (mod n 10) : acc
          | otherwise = go (div n 10) ((mod n 10) : acc)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" digitsArray
  where digitsArray = map digitToWord (digits n)


