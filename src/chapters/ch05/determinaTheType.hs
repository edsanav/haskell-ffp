{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where


example = 1

-- 54
-- Num p => p
a1 = (*9) 6

-- (0,"doge")
-- Num p => (p, [Char])
b1 =  head [(0,"doge"),(1,"kitteh")]

-- (0::Integer,"doge")
-- (Integer, [Char])
c1 = head [(0 :: Integer ,"doge"),(1,"kitteh")]

-- False
-- Bool
d1 =  if False then True else False

-- 5
-- Int
e1 = length [1, 2, 3, 4, 5]

-- False
-- Bool
f1 = (length [1, 2, 3, 4]) > (length "TACOCAT")


two = let x = 5
          y = x + 5
          w = y * 10
          in w

three = let x = 5
            y = x + 5
            z y = y * 10
            in z

four = let x = 5
           y = x + 5
           f = 4 / y
           in f

five = let x = "Julie"
           y = " <3 "
           z = "Haskell"
           f = x ++ y ++ z
           in f

dit1 = let bigNum = (^) 5 
           wahoo = bigNum $ 10
           in wahoo

dit3 = let a = (+)
           b = 5
           c = a 10
           d = c 200
           in d
             
dit4 c = let a = 12 + b
             b = 10000 * c
             in b
               
functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a ) => a -> a -> Bool 
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b  
functionS (x, y) = y