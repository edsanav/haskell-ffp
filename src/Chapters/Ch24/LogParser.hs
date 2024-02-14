{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Chapters.Ch24.LogParser where
import Control.Applicative

import Data.ByteString (ByteString)
import Text.RawString.QQ
import qualified Data.Map as M

import Chapters.Ch24.Utils (parseNDigits, rpbs, rp)

import Text.Trifecta

--example :: ByteString
--example = [r|
---- wheee a comment
--
-- # 2025-02-05
--08:00 Breakfast
--09:00 Sanitizing moisture collector
--11:00 Exercising in high-grav gym
--12:00 Lunch
--13:00 Programming
--17:00 Commuting home in rover
--17:30 R&R
--19:00 Dinner
--21:00 Shower
--21:15 Read
--22:00 Sleep
--
-- # 2025-02-07 -- dates not nececessarily sequential
--08:00 Breakfast -- should I try skippin bfast?
--09:00 Bumped head, passed out
--13:36 Wake up, headache
--13:37 Go to medbay
--13:40 Patch self up
--13:45 Commute home for rest
--14:15 Read
--21:00 Dinner
--21:15 Read
--22:00 Sleep
-- |]
-- 
exDate :: ByteString
exDate = [r|# 2025-02-07 -- dates not nececessarily sequential
|]

exEntry::ByteString
exEntry = [r|08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
|]

-- Maybe newtypes if we want to define show?
type Year = Integer
type Month = Integer
type Day = Integer

type Hour = Integer
type Minute = Integer
type Activity = String

data Date = Date Year Month Day deriving (Eq, Show, Ord)
data Time = Time Hour Minute deriving (Eq, Show, Ord)

type Entries = [(Time, Activity)]
newtype Log = Log (M.Map Date Entries) deriving (Eq, Show)

skipEOL::Parser()
skipEOL = skipMany (oneOf "\n")

skipComments:: Parser ()
skipComments =  skipMany (string "--" *> skipMany (noneOf "\n"))


parseTime:: Parser Date
parseTime = do
  _ <- string "# "
  year <- decimal
  _ <- char '-'
  month <- decimal
  _ <- char '-'
  day <- decimal
  return $ Date year month day
  
parseActivity::Parser Activity
parseActivity = undefined

parseEntry::Parser (Time, Activity)
parseEntry = do
  hours <- decimal
  _ <- char ':'
  minutes <- decimal
  _ <- char ' '
  activity <- some (noneOf ("\n" <|> "--" )) <* optional (char ' ') <* optional skipComments
  _ <- skipEOL
  return (Time hours minutes , activity)
  
main::IO()
main = do 
  print $ rpbs parseTime exDate
  print $ rpbs (some parseEntry) exEntry