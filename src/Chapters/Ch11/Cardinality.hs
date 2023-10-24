{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardinality where
  
import Data.Int

data Example = MakeExample deriving Show

data NullCardType = TheSingleValue deriving Show

data UnaryCardType = TheValue Int deriving Show

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

data NumberOrBool =Numba Int8 | BoolyBool Bool deriving (Eq, Show)


class TooMany a where
  tooMany :: a -> Bool
  
-- We can do this deriving TooMany because of the the GeneralizedNewtypeDeriving pragma and because 
-- there is an instance for the underlying type of the newtype (instance TooMany Int)  
newtype Cats = Cats Int deriving (Eq, Show, TooMany)

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany Int where
  tooMany n = n > 42
  
-- We can do this because of the FlexibleInstance pragma (and because there are instances for Int and String
instance TooMany (Int, String) where
  tooMany (i, _) = i > 42 

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42
  
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
  
instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (x,y) = tooMany (x + y) 



