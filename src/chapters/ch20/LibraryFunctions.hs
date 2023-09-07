module LibraryFunctions where
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product 

elem'::(Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap (\x -> Any(x==a)) ta

minimum':: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr minAd  Nothing
  where
    minAd a (Just b) = Just $ min a b
    minAd a Nothing = Just a
   
null'::(Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False ) True

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])
--toList' = foldMap (\x -> [x]) -- same as above

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)

foldMap'::(Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty