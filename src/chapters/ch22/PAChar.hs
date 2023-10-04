module PAChar where

import Data.Char


cap::[Char] -> [Char]
cap xs = map toUpper xs

rev ::[Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev.cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

{-
Prelude> composed "Julie"
"EILUJ"
Prelude> fmapped "Chris"
"SIRHC"
-}1

tupled::[Char] -> ([Char],[Char])
tupled = do
  capped <- cap
  reversed <- rev
  return (capped, reversed)

tupled'::[Char] -> ([Char],[Char])
tupled' = cap >>= (\capped -> rev >>= (\reversed -> return (capped, reversed)))