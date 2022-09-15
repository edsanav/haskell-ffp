module Cipher where

import Data.Char

letPos = [(l, ord l) | l <- concat $ repeat ['a'..'z']]
aLength = length ['a'..'z']

-- TODO do case > 'z', substract 26

caesar::String -> String
caesar "" = ""
caesar _ = undefined
