module Phone where

import Data.Char
import Data.List

data DaPhone = DaPhone [(Digit, String)]

sample =
  DaPhone
    [ ('1', "1"),
      ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "prqs7"),
      ('8', "tuv8"),
      ('9', "wxyz"),
      ('*', "^*"),
      ('0', " +_0"),
      ('#', ".,#")
    ]
-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

-- Use elemIndex to find position
-- use foldr
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c
  | isUpper c = (reverseTaps p '^')++(reverseTaps p (toLower c))
  | otherwise = undefined