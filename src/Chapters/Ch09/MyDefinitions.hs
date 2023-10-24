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

myMaximumBy:: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:xs) = case (compare x y) of
  LT -> myMaximumBy f (y:xs)
  _ -> myMaximumBy f (x:xs)

myMinimumBy:: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:y:xs) = case (compare x y) of
  GT -> myMinimumBy f (y:xs)
  _ -> myMinimumBy f (x:xs)

myGenericBy:: Ord a => Ordering -> (a -> a -> Ordering) -> [a] -> a
myGenericBy _ _ [] = error "empty list"
myGenericBy _ _ (x:[]) = x
myGenericBy o f (x:y:xs)
  | compare x y /= o = myGenericBy o f (y:xs)
  | otherwise = myGenericBy o f (x:xs)

myMaximumBy':: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy' = myGenericBy GT

myMinimumBy':: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy' = myGenericBy LT

myMaximum:: Ord a => [a] -> a
myMaximum = myMaximumBy' compare

myMinimum:: Ord a => [a] -> a
myMinimum = myMinimumBy' compare