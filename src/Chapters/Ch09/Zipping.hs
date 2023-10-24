module Zipping where


myZip:: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys


myZipWith::(a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y):myZipWith f xs ys


myZipWith2::(a -> b -> c)-> [a] -> [b] -> [c]
myZipWith2 f x y = map (uncurry f) (myZip x y)

myZip2:: [a] -> [b] -> [(a,b)]
myZip2 xs ys = myZipWith ((,)) xs ys