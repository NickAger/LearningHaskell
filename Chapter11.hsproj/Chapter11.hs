{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

import Data.Int
import Data.List
  
data Doggies a = 
    Huskey a
  | Mastif a
  deriving (Eq, Show)
  

data Price = Price Integer
            deriving (Eq, Show)

data Size = Size Integer
            deriving (Eq, Show)
            
data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)
  
data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)
  
data Vehicle = 
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)
  

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane (Car _ _) = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

data Example = MakeExample deriving Show

class TooMany a where 
  tooMany :: a -> Bool
  
instance TooMany Int where
  tooMany n = n > 42
  
-- error "constraint is not smaller than instance head in the constraint: Num a"
-- (Use UndecidableInstances to permit this)
--instance (Num a, Ord a, TooMany a) => TooMany a where
--  tooMany n = n > 42
  
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)

-- not needed as we add "TooMany" to the deriving clause possible with the language pragma - GeneralizedNewtypeDeriving
--instance TooMany Goats where--  tooMany (Goats n) = n > 43

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

instance TooMany (Int, String) where
  tooMany (n, a') = n + length a' > 42
  
--instance TooMany (Int, Int) where
--  tooMany (n1, n2) = n1 + n2 > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (n1, n2) = n1 + n2 > 42


data Test = TestAgain Int8


data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)
  
data JamJars = 
  Jam {flavour :: Fruit
      , quantity :: Int }
  deriving (Eq, Show)
  
instance Ord JamJars where
  compare a b = compare anum bnum
    where 
      anum = quantity a
      bnum = quantity b
  
row1 = Jam Peach 12
row2 = Jam Plum 10
row3 = Jam Apple 15
row4 = Jam Blackberry 20
row5 = Jam Apple 12
row6 = Jam Blackberry 14
allJam = [row1, row2, row3, row4, row5, row6]

totalJam :: [JamJars] -> Int
totalJam jars = sum $ map quantity allJam

totalJam2 :: [JamJars] -> Int
totalJam2 = foldr (\jam x -> x + quantity jam) 0

mostRow :: [JamJars] -> JamJars
mostRow = foldr1 (\jam mostJam -> if quantity jam > quantity mostJam then jam else mostJam)

mostRow2 :: [JamJars] -> JamJars
mostRow2 = last.sort

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortKinds :: [JamJars] -> [JamJars]
sortKinds = sortBy compareKind

groupJams :: [JamJars] -> [[JamJars]]
groupJams = (groupBy eqKinds).sortKinds
  where 
    eqKinds :: JamJars -> JamJars -> Bool
    eqKinds (Jam k _) (Jam k' _) = k == k' 
    

data GuessWhat =
  Chickenbutt deriving (Eq, Show)
  
data Id a =
  MkId a deriving (Eq, Show)
  
data Product a b =
  Product a b deriving (Eq, Show)
  
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)
  
data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b}
                deriving (Eq, Show)
                
newtype NumCow =
  NumCow Int
  deriving (Eq, Show)
  
newtype NumPig =
  NumPig Int
  deriving (Eq, Show)
  
data Farmhouse =
  Farmhouse NumCow NumPig
  
type Formhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)
  
data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  
type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)
  
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)
  
data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)
  
data SheepInfo = 
  SheepInfo Name Age PoundsOfWool
  deriving (Show, Eq)
  
data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)
  
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)
  


