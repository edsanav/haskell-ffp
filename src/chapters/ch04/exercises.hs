module Ch04.Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

f::(a,b) -> (c,d) -> ((b,d),(a,c))
f t1 t2 = ((snd t1, snd t2),(fst t1, fst t2)) 

xx = (+)

ff :: [a] -> Int
ff xs = w `xx` 1 
  where w = length xs
  
fff x = x
  
fAgain:: (a,b) -> a
fAgain (a, _) = a