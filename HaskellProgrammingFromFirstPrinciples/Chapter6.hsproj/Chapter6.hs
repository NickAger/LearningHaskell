module Chapter6 where
  
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

--
  
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = v1 == v1' && v2 == v2'
  
--

data StringOrInt = 
    TisAnInt Int
  | TisAString String
  
instance Eq StringOrInt where
  (==) (TisAnInt v1) (TisAnInt v1') = v1 == v1'
  (==) (TisAString v1) (TisAString v1') = v1 == v1' 
  (==) _ _ = False
  
--

data Pair a = 
  Pair a a
  
instance Eq a => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v1' v2') = v1 == v1' && v2 == v2'

--

data Tuple a b = 
  Tuple a b
  
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v1 v2) (Tuple v1' v2') = v1 == v1' && v2 == v2'


--

data Which a =
    ThisOne a
  | ThatOne a
  
instance Eq a => Eq (Which a) where
  (==) (ThisOne v1) (ThisOne v1') = v1 == v1'
  (==) (ThatOne v1) (ThatOne v1') = v1 == v1' 
  (==) _ _ = False 
  
--

data EitherOr a b = 
    Hello a
  | Goodbye b
  
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v1) (Hello v1') = v1 == v1'
  (==) (Goodbye v1) (Goodbye v1') = v1 == v1'
  (==) _ _ = False   
  

--

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

--

data Mood = Blah
        |   Woot deriving Show
        
instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _ = False

settleDown x = if x == Woot
                then Blah
                else x
     
--

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)


i :: a
i = 1

f :: RealFrac a => a
f = 1.0

myX = 1 :: Int
sigmound :: Num a => a -> a
sigmound x = myX
