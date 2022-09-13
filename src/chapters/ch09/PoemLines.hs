module PoemLines where


myWords::String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords a = (takeWhile (/=' ') a):myWords( dropWhile(/=' ') a)

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines x = (takeWhile (/='\n') x):myLines (dropWhile(/='\n') x)

myLinesGen:: Char -> String -> [String]
myLinesGen _ [] = []
myLinesGen c l@(x:xs)
  | c == x = myLinesGen c xs
  | otherwise = (takeWhile (/=c) l):myLinesGen c (dropWhile(/=c) l)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?\n"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = [
  "Tyger Tyger, burning bright",
  "In the forests of the night",
  "What immortal hand or eye",
  "Could frame thy fearful symmetry?"]

run::IO()
run = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)