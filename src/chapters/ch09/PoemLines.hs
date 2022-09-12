module PoemLines where


myWords::String -> [String]
myWords [] = []
myWords (' ':xs) = myWords (dropWhile (==' ') xs)
myWords a = (takeWhile (/=' ') a):myWords( dropWhile(/=' ') a)

myLines :: String -> [String]
myLines [] = []
myLines ('\\':'n':xs) = myLines xs
myLines('\\':xs) = myLines xs
myLines x = (takeWhile (/='\\') x):myLines (dropWhile(/='\\') x) 