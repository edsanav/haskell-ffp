module SmallMaybe where

isJust::Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing::Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee::b -> (a->b) -> Maybe a -> b
mayybee _ f (Just a) =  (f a)
mayybee b _ Nothing = b
  
fromMaybe:: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe df Nothing = df

listToMaybe::[a]-> Maybe a
listToMaybe (x:_) = Just x
listToMaybe [] = Nothing

maybeToList::Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes::[Maybe a]->[a]
catMaybes [] = []
catMaybes (Just a:xs) = (a:catMaybes xs)
catMaybes (Nothing:xs) = catMaybes xs

flipMaybe::[Maybe a]-> Maybe [a]
flipMaybe xs 
  | length catted == length xs = Just catted
  | otherwise = Nothing
  where catted = catMaybes xs
 
  