module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF::(Show a, Read a) => a -> a
roundTripPF = read . show

roundTripOther::(Show a, Read b) => a -> b 
roundTripOther = read . show


main = do
  print (roundTripPF 4)
  print (id 4)
  print (roundTripOther 4::Int)