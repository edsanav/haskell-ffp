module Main where

import Hello
import DogsRule
import Scratch
import System.IO

main :: IO()
main = do
  hSetBuffering stdout NoBuffering -- If removed, it doesn't print what written
  putStrLn "Please input your name: "
  name <- getLine
  sayHello name
  dogs
  twooPrint