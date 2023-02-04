module VigCipher (vigne,unvigne, caesar, uncaesar) where

import Data.Char

  
data Direction = LeftShift | RightShift

-- this is copied from ch9 (should use proper module structure)
shiftChar :: Direction -> Int -> Char -> Char
shiftChar _ _ ch | not $ elem (toLower ch) ['a'..'z'] = ch
shiftChar d n y = chr finalRes
  where
    originalRes = op (ord y) (mod n 26)
    finalRes
      | comp (originalRes) (limit) = adjustment originalRes
      | otherwise = originalRes
    (op, comp, limit, adjustment) = case d of
      RightShift -> ((+), (>), (ord 'z'), (\x -> (-) x 26))
      LeftShift -> ((-), (<), (ord 'a'), (\x -> (+) x 26))


getShift::Char -> Int
getShift x
 | elem (toLower x) ['a'..'z'] = (ord (toLower x) - ord 'a')
 | otherwise = 0

zipWithSpaces:: String -> String -> [(Char, Char)]
zipWithSpaces "" _ = []
zipWithSpaces _ "" = []
zipWithSpaces (' ':xs) xy = (' ',' '):zipWithSpaces xs xy
zipWithSpaces (x:xs) (y:ys) = (x,y):zipWithSpaces xs ys


shiftWords::Direction -> String -> String -> String
shiftWords d w ref = map (uncurry ciph) zipped
  where zipped = zipWithSpaces w (concat $ repeat ref)
        ciph a r = shiftChar d (getShift r) (toLower a)

vigne::String -> String -> String
vigne = shiftWords RightShift

unvigne::String -> String -> String
unvigne = shiftWords LeftShift

shiftString :: Direction -> String -> Int -> String
shiftString _ "" _  = ""
shiftString d (x : xs) n
  | elem x ['a' .. 'z'] = (shiftChar d n x) : (shiftString d xs n)
  | otherwise = x : (shiftString d xs n)

caesar ::  String -> Int -> String
caesar = shiftString RightShift

uncaesar :: String -> Int -> String
uncaesar = shiftString LeftShift

