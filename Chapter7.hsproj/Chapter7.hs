module Chapter7 where
  
import Control.Monad

-- functionC x y = if (x < y) then x else y

functionC x y = 
  case (x < y) of
    True -> x
    False -> y
    
-- ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2 n = 
  case even n of 
    True -> n+2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
    
dodgy :: Num a => a -> a -> a 
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> (f (g a))



addOne :: Int -> Int
addOne = \x -> x + 1


tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10
       

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10
        

hunsD x = d2
  where d = x `div` 100
        d2 = d `mod` 10
        


foldBool :: a -> a -> Bool -> a
foldBool = undefined

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBool0 :: a -> a -> Bool -> a
foldBool0 x y p = if p then x else y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y p = 
  case p of
    True -> x
    False -> y
  
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y p
  | p == True = x
  | otherwise = y
  
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show

data Product a b =
  Product a b
  deriving (Eq, Show)
  
-- productUnpack :: Product a b -> (a, b)
-- productUnpack (Product x x) = (x, x)

f x = f x

--wtf d = zipWith (+) (\ l -> (map d l) >>= \h -> h)

--wtf2 = zipWith (+) . (join .) . map

