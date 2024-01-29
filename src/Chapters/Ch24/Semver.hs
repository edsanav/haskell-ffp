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
parsePart = do
  res <- (Left <$> integer) <|> (Right <$> some letter)
  case res of
    Left i -> return (NOSI i)
    Right an -> return (NOSS an)

parseEnding::Parser [NumberOrString]
parseEnding = some $ parsePart <* skipDot

parseSemVer:: Parser SemVer
parseSemVer = do
  maj <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  return (SemVer maj minor patch [] [])



main:: IO ()
main = do
  let p f i = parseString f mempty i
--  print $ p parseSemVer "1.2.4"
  print $ p parseEnding "12.23" -- this works ATM
  print $ p parseEnding "12.23.123a2" -- this doesnt (maybe use notFollowedBy?)
  print "Done"