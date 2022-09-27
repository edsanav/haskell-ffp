module Cipher where

import Data.Char

letPos = [(l, ord l) | l <- concat $ repeat ['a' .. 'z']]

data Direction = LeftShift | RightShift

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

cipherAlone :: Int -> Char -> Char
cipherAlone n y = chr finalRes
  where
    originalRes = ord y + mod n 26
    finalRes
      | originalRes > (ord 'z') = originalRes - 26
      | otherwise = originalRes

uncipherAlone :: Int -> Char -> Char
uncipherAlone n y = chr finalRes
  where
    originalRes = ord y - mod n 26
    finalRes
      | originalRes < (ord 'a') = originalRes + 26
      | otherwise = originalRes

cipher :: Int -> Char -> Char
cipher = shiftChar RightShift

uncipher :: Int -> Char -> Char
uncipher = shiftChar LeftShift

shiftString :: Direction -> Int -> String -> String
shiftString _ _ "" = ""
shiftString d n (x : xs)
  | elem x ['a' .. 'z'] = (shiftChar d n x) : (shiftString d n xs)
  | otherwise = x : (shiftString d n xs)


caesar :: Int -> String -> String
caesar = shiftString RightShift 

unCaesar :: Int -> String -> String
unCaesar = shiftString LeftShift 

