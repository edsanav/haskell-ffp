{-# LANGUAGE LambdaCase #-}

module LibraryFunctions where
import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product 

elem::(Foldable t, Eq a) => a -> t a -> Bool
elem a ta = getAny $ foldMap (\x -> Any(x==a)) ta

minimumA:: (Foldable t, Ord a) => t a -> Maybe a
minimumA ta 
 | null ta = Nothing
 | otherwise = foldr minAd  Nothing ta
  where
    minAd a (Just b) = Just $ min a b
    minAd a Nothing = Just a
   