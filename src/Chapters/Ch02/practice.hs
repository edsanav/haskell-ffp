module Mult1 where

mult1 = x * y
  where x = 5
        y = 6

mult2 = x * 3 + y
  where x = 3
        y = 1000

mult3 = x * 5
  where y = 10
        x = 10 * 5 + y

mult4 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff x = triple x
