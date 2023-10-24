module Example where

import System.Random

a = mkStdGen 0 :: StdGen -- two Int32 values

-- next produces Int (random value) and a StdGen
b = next(a)

c = snd b

d = random a:: (Char, StdGen)

