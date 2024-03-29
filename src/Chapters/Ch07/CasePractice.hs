module CasePractice where

functionCRaw x y = if (x > y) then x else y
 
functionC x y =
  case x > y of 
    True -> x
    False -> y 
    
ifEvenAdd2Raw n = if even n then (n+2) else n
  
ifEvenAdd2 n =
  case isEven of 
    True -> n + 2
    False -> n
  where isEven = even n
  
nums x =
  case compare x 0 of 
    LT -> -1 
    GT -> 1
    EQ -> 0