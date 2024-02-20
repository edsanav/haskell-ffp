module Chapters.Ch24.Exercises where
import Control.Applicative

import Chapters.Ch24.Utils (rp, parseNDigits)

import Text.Trifecta
import Data.Word
import Data.Bits
  
import Data.Char (digitToInt, toUpper)

hexToDecimal :: String -> Int
hexToDecimal = sum . zipWith (*) (iterate (*16) 1) . reverse . map (digitToInt .toUpper)

parseDigit :: Parser Char
parseDigit = choice $ fmap char "0123456789"

base10Integer :: Parser Integer
base10Integer = option (1::Integer) ((-1::Integer) <$ char '-') >>= (\mult -> (*mult) <$> (read <$> some parseDigit)) 

base10Integer' :: Parser Integer
base10Integer' = do 
  mult  <- option (1::Integer) ((-1::Integer) <$ char '-')
  digis <- some parseDigit
  return $ mult * foldl (\b a -> b*10 + read [a]) 0 digis

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)


parseNumberPlanArea::Parser NumberingPlanArea
parseNumberPlanArea = do
   _ <- optional (string "1-")
   try (between (char '(') (char ')') (parseNDigits 3)) <|> parseNDigits 3

parsePhone::Parser PhoneNumber
parsePhone = do
  area <- parseNumberPlanArea
  _ <- optional $ try (char '-') <|> try (char ' ')
  ex <- parseNDigits 3
  _ <- optional (char '-')
  ln <- parseNDigits 4
  return $ PhoneNumber area ex ln

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

parseIPv4::Parser IPAddress
parseIPv4 = do 
  firstOct <- decimal
  _ <- char '.'
  secondOct <- decimal
  _ <- char '.'
  thirdOct <- decimal
  _ <- char '.'
  forthOct <- decimal
  return $ IPAddress (sum $ map fromInteger [
    firstOct `shiftL` 24, 
    secondOct `shiftL` 16, 
    thirdOct `shiftL` 8, 
    forthOct `shiftL` 0]
    )

newtype IPAddress6 = IPAddress6 Word64 deriving (Eq, Ord, Show)

-- TODO finish me. Those are not decimals!

parseIPv6::Parser IPAddress6
parseIPv6 = do
  f1 <-  decimal
  _ <- char ':'
  f2 <-  decimal
  _ <- char ':'
  f3 <-  decimal
  _ <- char ':'
  f4 <-  decimal
  _ <- char ':'
  f5 <-  decimal
  _ <- char ':'
  f6 <-  decimal
  _ <- char ':'
  f7 <-  decimal
  _ <- char ':'
  f8 <-  decimal
  _ <- char ':'
  return $ IPAddress6(sum $ map fromInteger [
    f1 `shiftL` 112,
    f2 `shiftL` 96,
    f3 `shiftL` 80,
    f4 `shiftL` 64,
    f5 `shiftL` 48,
    f6 `shiftL` 32,
    f7 `shiftL` 16,
    f8 `shiftL` 8
    ])

main:: IO ()
main = do
  print $ rp parseDigit "123"
  print $ rp parseNumberPlanArea "1-(123)"
  print $ rp parseNumberPlanArea "(123)"
  print $ rp parseNumberPlanArea "1-(123)4"
  print $ rp parseNumberPlanArea "1-1234"
  print $ rp parsePhone "123-456-7890"
  print $ rp parsePhone "1234567890"
  print $ rp parsePhone "(123) 456-7890"
  print $ rp parsePhone "1-123-456-7890"
  print $ rp parseIPv4 "172.16.254.1"
  print $ rp parseIPv4 "204.120.0.15"
  print $ rp parseIPv6 ""
  print "Done"