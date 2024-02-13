module Chapters.Ch24.Exercises where
import Control.Applicative

import Chapters.Ch24.Utils (rp, parseNDigits)

import Text.Trifecta
  
  


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
  print "Done"