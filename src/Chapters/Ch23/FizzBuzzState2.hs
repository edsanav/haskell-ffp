module Chapters.Ch23.FizzBuzzState2 where

import Chapters.Ch23.FizzBuzzState (fizzBuzz)
import Control.Monad.State

fizzBuzzFromTo::Integer -> Integer -> [String]
fizzBuzzFromTo x y = execState (mapM addResultRev reversedL) []
  where reversedL = if x > y then [y..x] else [y, (y-1)..x]  

addResultRev::Integer -> State [String] ()
addResultRev n = do
    xs <- get
    let result = fizzBuzz n
    put (result:xs)

