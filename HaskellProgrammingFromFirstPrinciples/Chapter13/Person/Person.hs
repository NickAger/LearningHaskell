import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                      | AgeTooLow
                      | PersonInvalidUnknown String
                      deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine

  let person = mkPerson name (read age)
  putStrLn $ formatPerson person

  where
    formatPerson :: Either PersonInvalid Person -> String
    formatPerson (Right person) = "Yay! Successfully got a person: " ++ (show person)
    formatPerson (Left error) = "An error occured creating a person. Error: " ++ (show error)
