module DefinitionsFold where

myAnd::[Bool] -> Bool
myAnd = foldr (&&) (True)

myOr::[Bool]->Bool
myOr = foldr (||) (False)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False 

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False 

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f(a) then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++). f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


decideFn :: (a -> a -> Ordering) -> Ordering -> a -> a -> a
decideFn f o a b
    | f a b == o = a
    | otherwise = b

-- z needs to be last element must be last element in list to compare, otherwise it fails on this case
--Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Instead it returns 2

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f xs =  foldr (decideFn f GT) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f xs =  foldr (decideFn f LT) (last xs) xs
