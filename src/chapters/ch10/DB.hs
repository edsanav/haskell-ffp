module DB where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr extractor []
  where
    extractor a b = case a of
      DbDate x -> x : b
      _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr extractor []
  where
    extractor a b = case a of
      DbNumber x -> x : b
      _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = getMostRecent . filterDbDate
  where
    getMostRecent xs =
      foldr
        (\a b -> if (a > b) then a else b)
        (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
        xs

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = maximum . filterDbDate

sumDb::[DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- TODO: improve this 
avgDb :: [DatabaseItem] -> Double
avgDb xs = (\x -> fromIntegral (  div (sumDb x) (toInteger $ length (filterDbNumber xs)))) xs
