{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Chapters.Ch24.Semver where

import Control.Applicative
import Data.ByteString (ByteString)

import Test.Hspec
import Text.RawString.QQ

import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)


skipDot::Parser ()
skipDot = skipMany (char '.')

parsePart::Parser NumberOrString
parsePart = NOSI <$> try (integer <* notFollowedBy letter) <|> NOSS <$> some alphaNum

parseEndSection::Parser [NumberOrString]
parseEndSection = many $ parsePart <* skipDot

parseEnd::Parser ([NumberOrString], [NumberOrString])
parseEnd = do
  first <- option [] (char '-' *> parseEndSection)
  second <- option [] (char '+' *> parseEndSection)
  return (first, second)

parseSemVer:: Parser SemVer
parseSemVer = do
  maj <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  (prer, meta) <- parseEnd
  _ <- eof
  return (SemVer maj minor patch prer meta)



main:: IO ()
main = do
  let p f i = parseString f mempty i
  print $ p parseSemVer "1.2.4"
  print $ p parseSemVer "1.0.0-x.7.z.92"
  print $ p parseSemVer "1.0.0-gamma+002" -- this doesn't work yet
  print $ p parseSemVer "1.0.0-beta+oof.sha.41af286"
  print "Done"