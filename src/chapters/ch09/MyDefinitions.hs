module MyDefinitions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) =
  if x == False
    then False
    else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : _) = True
myOr (False : xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = case f x of
  True -> True
  False -> myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys
  
myElemAny ::  Eq a => a -> [a] -> Bool
myElemAny x xs = any (== x) xs

myReverse:: [a] -> [a]
myReverse l = go l []
  where go [] a = a
        go (x:xs) a = go xs (x:a)
        
squish::[[a]] -> [a]
squish [] = []
squish ([]:lls) = squish lls
squish ((x:xs):lls) = x:squish (xs:lls)


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMap2:: (a -> [b]) -> [a] -> [b]
squishMap2 _ [] = []
squishMap2 fn l = squish (ma fn l [])
  where ma _ [] a = reverse a
        ma f (x:xs) a = ma f xs ((f x):a)
        
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 