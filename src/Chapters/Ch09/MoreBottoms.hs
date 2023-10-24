module MoreBottoms where

import Data.Bool (bool)



foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    False -> x
    True -> y

myFunc::(Num a, Eq a) => [a] -> [a]
myFunc = map  (\x -> bool (-x) x (x==3))

