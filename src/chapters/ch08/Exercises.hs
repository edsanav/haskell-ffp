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