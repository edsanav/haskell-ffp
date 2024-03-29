{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch24.BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString(parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP::(Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP::Show a => A.Parser a -> ByteString -> IO()
attoP p i = print $ parseOnly p i

-- This does not backtrack (if failing by the first, it won't try to go back whats advanced)
nobackParse::(Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

-- This backtracks (moves the cursor to starting point, where it was before failed parser consumed input)
tryParse::(Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

-- This is as the previous but annotating in case of failure (so we know what parser tried)1
tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "13"
  trifP tryParse "13"
  trifP tryAnnot "13"

  -- parsec
  parsecP nobackParse "13"
  parsecP tryParse "13"
  parsecP tryAnnot "13"

  -- attoparsec
  attoP nobackParse "13"
  attoP tryParse "13"
  attoP tryAnnot "13"