module As where

import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (_:_) [] = False
isSubseqOf a@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map cap . words 
  where cap [] = ("", "")
        cap a@(x:xs) = ((toUpper x:xs),a)