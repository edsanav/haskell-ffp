module VigCipher where

import Data.Char

  
data Direction = LeftShift | RightShift

-- this is copied from ch9 (should use proper module structure)
shiftChar :: Direction -> Int -> Char -> Char
shiftChar _ _ ' ' = ' ' -- spaces do not move
shiftChar d n y = chr finalRes
  where
    originalRes = op (ord y) (mod n 26)
    finalRes
      | comp (originalRes) (limit) = adjustment originalRes
      | otherwise = originalRes
    (op, comp, limit, adjustment) = case d of
      RightShift -> ((+), (>), (ord 'z'), (\x -> (-) x 26))
      LeftShift -> ((-), (<), (ord 'a'), (\x -> (+) x 26))

--cipher :: Int -> Char -> Char
--cipher = shiftChar RightShift
--
--uncipher :: Int -> Char -> Char
--uncipher = shiftChar LeftShift

getShift::Char -> Int
getShift x
 | elem (toLower x) ['a'..'z'] = (ord (toLower x) - ord 'a')
 | otherwise = 0

getShifts :: String -> [Int]
getShifts = map getShift

cifRef:: String -> [Int]
cifRef = concat.repeat.getShifts

cipherChar:: [Int] -> Int -> Char -> Char
cipherChar _ _ ' ' = ' '
cipherChar ref idx x = shiftChar RightShift (ref !! idx) x

zipWithSpaces:: String -> String -> [(Char, Char)]
zipWithSpaces "" _ = []
zipWithSpaces _ "" = []
zipWithSpaces (' ':xs) xy = (' ',' '):zipWithSpaces xs xy
zipWithSpaces (x:xs) (y:ys) = (x,y):zipWithSpaces xs ys


shiftWords::Direction -> String -> String -> String
shiftWords d w ref = map (uncurry ciph) zipped
  where zipped = zipWithSpaces w (concat $ repeat ref)
        ciph a r = shiftChar d (getShift r) (toLower a)

encode::String -> String -> String
encode = shiftWords RightShift

decode::String -> String -> String
decode = shiftWords LeftShift
