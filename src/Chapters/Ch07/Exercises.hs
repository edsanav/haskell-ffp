module Exercises where

first :: Char -> String
first x = x : x : []

sec :: String -> [String]
sec x = [x, x, x]

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = mod (fst $ divMod x 10) 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where
    xLast = x `div` 100
    d2 = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b 
  | b == False = x
  | otherwise = y

myG :: (a->b) -> (a, c) -> (b, c)
myG f t = (f . fst $ t, snd t)