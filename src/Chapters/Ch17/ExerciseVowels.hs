module ExerciseVowels where

import Control.Applicative

stops = "pbtdkg"
vowels = "aeiou"
other = "vwxyz"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)  
