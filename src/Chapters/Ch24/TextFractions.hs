{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch24.TextFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Numeric

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

parseFraction::Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)
  
virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

---- Use try because otherwise it will start matching the first option and won't bother wit the second
fractionOrInt::Parser (Either Rational Integer)
fractionOrInt = (Left <$> try virtuousFraction) <|> (Right <$> try integer) <* eof

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' = parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork