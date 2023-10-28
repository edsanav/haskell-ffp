module Chapters.Ch23.FizzBuzzState where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL


fizzBuzz:: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer]-> DL.DList String
fizzbuzzList list = execState (mapM_ addResult list) DL.empty


-- https://wiki.haskell.org/State_Monad
addResult::Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-- mapM is like traverse
main :: IO ()
main =
  mapM_ putStr $ fizzbuzzList [1..100]