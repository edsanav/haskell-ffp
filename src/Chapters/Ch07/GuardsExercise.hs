module GuardsExercise where



avgGrade:: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100
  
pal::(Eq a) => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False
  
numbers::(Ord a, Num a, Num p) => a -> p
numbers x
 | x < 0 = -1
 | x == 0 = 0
 | otherwise = 1