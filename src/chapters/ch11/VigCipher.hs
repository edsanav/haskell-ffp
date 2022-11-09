module VigCipher where

import Data.Char

  
data Direction = LeftShift | RightShift

-- this is copied from ch9 (should use proper module structure)
shiftChar :: Direction -> Int -> Char -> Char
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

getShifts :: String -> [Int]
getShifts "" = []
getShifts (x:xs) = (ord (toLower x) - ord 'a'):getShifts(xs) 

cifRef:: String -> [Int]
cifRef = concat.repeat.getShifts

cipherChar:: [Int] -> Int -> Char -> Char
cipherChar _ _ ' ' = ' '
cipherChar ref idx x = shiftChar RightShift (ref !! idx) x

-- TODO skip spaces 