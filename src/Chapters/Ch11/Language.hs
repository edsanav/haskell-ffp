module Language where
import Data.Char
import Data.List (isSuffixOf)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x:xs

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . capi . words
   where 
      capi [] = []
      capi (lastw:[]) = lastw:[]
      capi (x:y:ys)
        | isSuffixOf "." x = x:capitalizeWord y:capi(ys)
        | otherwise = x:y:capi(ys)



