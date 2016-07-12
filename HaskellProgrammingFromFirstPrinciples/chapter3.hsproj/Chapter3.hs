module Reverse where
  
rvrs :: String -> String
rvrs x = concat [awesome, " ", is, " ", curry]
  where
      awesome = drop 9 x
      curry = take 5 x
      is = take 2 $ drop 6 x
      

main :: IO ()
main = print $ rvrs "Curry is awesome"


data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x


myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x


f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

f1 :: [a] -> Int
f1 xs = w `x` 1
  where w = length xs
 

f2 (a,b) = a
