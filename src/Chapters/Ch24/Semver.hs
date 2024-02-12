{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch24.Semver where

import Control.Applicative

import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSS y) = compare x y
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS _) (NOSI _) = GT

-- mappend to compose compare results: mappend EQ LT = LT
instance Ord SemVer where
  compare (SemVer mj1 mi1 p1 [] _) (SemVer mj2 mi2 p2 [_] _) =
     foldl mappend EQ [compare mj1 mj2, compare mi1 mi2, compare p1 p2, GT]
  compare (SemVer mj1 mi1 p1 [_] _) (SemVer mj2 mi2 p2 [] _) =
     foldl mappend EQ [compare mj1 mj2, compare mi1 mi2, compare p1 p2, LT]
  compare (SemVer mj1 mi1 p1 r1 m1) (SemVer mj2 mi2 p2 r2 m2) =
    foldl mappend EQ [compare mj1 mj2, compare mi1 mi2, compare p1 p2, compare r1 r2, compare m1 m2 ]


skipDot::Parser ()
skipDot = skipMany (char '.')

-- careful, use decimal not integer because otherwise "+" sign will be considered part of the number
parsePart::Parser NumberOrString
parsePart = NOSI <$> try (decimal <* notFollowedBy letter) <|> NOSS <$> some alphaNum

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