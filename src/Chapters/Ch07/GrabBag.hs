module GrabBag where

  
m1 x y z = x * y * z

m2 x y = \z -> x * y * z

m3 x = \y -> \z -> x * y * z

m4 = \x -> \y -> \z -> x * y * z


addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1 
  
addFive = \x -> \y -> (if x > y then y else x) + 5


mFlip f x y = f y x