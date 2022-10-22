module Exercises where

stops = "pbtdkg"
vowels = "aeiou"


combineAll::String -> String -> [(Char, Char, Char)]
combineAll xs xv = [(s,v,s') | s <- xs, v<- xv, s' <-xs ]

combineP::String -> String -> [(Char, Char, Char)]
combineP xs xv = [(s,v,s') | s <- xs, s =='p', v<- xv, s' <-xs ]

nouns = ["table","computer","pen","dinosaur","boy","pizza"]
verbs = ["play","watch","see","ride","eat","drink"]

combineG::[a] -> [b] -> [(a,b,a)]
combineG xs xv = [(s,v,s') | s <- xs, v<- xv, s' <-xs ]

combineGFilt:: (a -> Bool) -> [a] -> [b] -> [(a,b,a)]
combineGFilt f xs xv = [(s,v,s') | s <- xs, f s, v<- xv, s' <-xs ]

seekritFunc x = (fromIntegral (sum (map length (words x))))  / (fromIntegral (length (words x)))
