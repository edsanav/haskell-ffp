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
