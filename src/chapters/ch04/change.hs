module Change where

data Mood = Whoot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Whoot = Blah
changeMood    _ = Whoot