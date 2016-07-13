import Data.Monoid
import Control.Monad.Writer

-- page 569
 
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")
 
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
--applyLog (x, log) f = 
--  let (y, newLog) = f x 
--  in (y, log `mappend` newLog)
applyLog (x, log) f = 
  let (y, newLog) = f x 
  in (y, log <> newLog)  


testMonoid :: (Monoid m) => m -> m
testMonoid a = mempty

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans"  = ("milk", Sum 25)
addDrink "jerky"  = ("whiskey", Sum 99)
addDrink  _       = ("beer", Sum 30)

-- page 231

-- data Car = Car String String Int deriving (Show) 
data Car = Car {company :: String,
                model :: String,
                year :: Int
                } deriving (Show)

        
data AlternativeCar = SpecialConstructorCar {
                company' :: String,
                model' :: String,
                year' :: Int
                } deriving (Show)
-- page 574

--instance (Monoid w) => Monad (Writer w) where 
--  return x = Writer (x, mempty)
--  (Writer (x, v)) >>= f = 
--    let (Writer (y, v')) = f x
--    in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)
  
  
    


