module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy:: String -> String -> String
flippy = flip cattyConny

appedCatty::String -> String
appedCatty = cattyConny "woops"

frappe::String -> String
frappe = flippy "ha ha"

mySum::(Eq a, Num a) => a -> a
mySum x 
  | x == 1 = 1
  | otherwise = x + (mySum $ x -1)

mySumCom::(Eq a, Num a) => a -> a
mySumCom x 
  | x == 1 = 1
  | otherwise = (+x) . mySum $ x - 1
  
myMult::(Integral a) => a -> a -> a
myMult x y = case sameSign of
  True -> go x y (abs y)
  False -> negate $ go x y (abs y)
  where go a b count
          | count == 0 = 0
          | otherwise = (abs a) + (go a b (count-1))
        sameSign = (x >= 1 && y >= 1) || (x <= (-1) && y<=(-1))
        
data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy:: Integral a => a -> a -> DividedResult
dividedBy num denom = case (sameSign, result) of
  (_, DividedByZero) -> DividedByZero
  (True, res) ->  res
  (False, Result x) -> Result $ negate x 
  where go n d count
         | d == 0 = DividedByZero
         | n < d = Result (count)
         | otherwise = go (n - d) d (count  + 1)
        result = go (abs num) (abs denom) 0 
        sameSign = (num >= 1 && denom >= 1) || (num <= (-1) && denom <=(-1))
        
        
mc91::Integral a => a -> a
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ (x + 11)