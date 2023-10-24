module Examples where
import Control.Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s
   
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)
mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

-- step by step on why you can do Person <$> mkName "bla" <*> mkAddress "st"
bla ::Maybe (Address -> Person)
bla  = Person <$> mkName "bla"

addr::Maybe Address 
addr = mkAddress "sesamo st"

finalP :: Maybe Person
finalP = bla <*> addr

-- can also be done with
-- like a functor with two parameters?
alt:: Maybe Person
alt = liftA2 Person (mkName "bla") (mkAddress "sesamo st")