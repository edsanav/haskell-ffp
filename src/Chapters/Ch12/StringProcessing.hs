module StringProcessing where

import Data.List (intercalate)
import Data.List (partition)


notThe::String -> Maybe String
notThe "the" = Nothing
notThe x = Just x


replaceThe::String -> String
replaceThe = intercalate " ".doReplace.words
  where doReplace [] = [""]
        doReplace (x:xs)
         | notThe x == (Just x) = x:doReplace xs
         | otherwise = "a":doReplace xs

isVowel::Char -> Bool
isVowel c  = elem c "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = (go 0).words
  where go n [] = n
        go n (_:[]) = n
        go n (_:[]:xs) = go n xs
        go n (x:(c:_):xs)
         | notThe x == Nothing && isVowel c = go (n+1) xs
         | otherwise = go n xs

countVowels::String->Int
countVowels = length.(filter isVowel)

newtype Word' = Word' String deriving (Eq,Show)

mkWord::String -> Maybe Word'
mkWord myS
  | compareParts $ partition isVowel myS = Just (Word' myS)
  | otherwise = Nothing
  where compareParts (x,y) = length x > length y
        
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1+ natToInteger(n)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n | n < 0 = Nothing
integerToNat n = Just (nextStep n)
  where nextStep 0 = Zero
        nextStep x = Succ (nextStep (x-1))

