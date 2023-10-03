module PartiallyApplied where

import Control.Applicative

boop = (* 2)

doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

-- the structure is a partially applied function here
bloop :: Integer -> Integer
bloop = fmap boop doop

-- in applicative context, here the arguments gets passed in parallel
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

{-
((+) <$> (*2) <*> (+10)) 3

-- First the fmap
(*2) :: Num a => a -> a
(+) :: Num a => a -> a -> a
(+) <$> (*2) :: Num a => a -> a -> a
Mapping a function awaiting two arguments over a function await-
ing one produces a two argument function.

(+) . (*2) :: Num a => a -> a -> a

((+) <$> (*2) <*> (+10)) 3
((+) <$> (*2) <*> (+10)) :: Num b => b -> b

((+) <$> (*2) <*> (+10)) 3
(3*2) + (3+10)
6 + 13
19
-}

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- In moadic context:
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
