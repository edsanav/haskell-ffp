module Scans where

fibs = 1 : scanl (+) 1 fibs

first20Fibs::[Integer]
first20Fibs = take 20 fibs

lowerFibs::[Integer]
lowerFibs = takeWhile ( < 100) fibs

factoFold:: Integer -> Integer
factoFold n =  foldl (*) 1 [1..n]

--  scanl (\b a-> a:b)  [1] [1..10] multiply this
facto::[Integer]
facto = undefined