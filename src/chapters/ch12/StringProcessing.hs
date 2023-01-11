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
        