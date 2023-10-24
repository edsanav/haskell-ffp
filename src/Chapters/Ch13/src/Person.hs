module Person (gimmePerson) where

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)


mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please give me name for the person"
  name <- getLine
  putStrLn "Please give me an age for the person"
  age <- readLn
  case mkPerson name age of
    Right p -> putStrLn $ "Successfully got a person "++ (show p)  
    Left e -> putStrLn $ "Invalid input: " ++ (show e)
  