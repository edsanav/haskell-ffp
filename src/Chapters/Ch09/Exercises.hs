module Exercises where

import Data.Char

capFirst::String -> String
capFirst [] = []
capFirst (x:xs) = toUpper x:xs

capAll::String -> String
capAll [] = []
capAll (x:xs) = toUpper x:capAll xs

onlyFirstCap::String -> Char
onlyFirstCap = toUpper . head 