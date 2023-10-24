module Main where

import Palindrome (palindrome)
import Person (gimmePerson)

import Hello
import DogsRule
import PartiallyApplied
import System.IO

main :: IO()
main = do
  hSetBuffering stdout NoBuffering -- If removed, it doesn't print what written
  putStrLn "Please input your name: "
  name <- getLine
  sayHello name
  dogs
  twooPrint


mainPal::IO()
mainPal = do
   hSetBuffering stdout NoBuffering -- If removed, it doesn't print what written
   palindrome

mainPerson::IO()
mainPerson = do
   hSetBuffering stdout NoBuffering -- If removed, it doesn't print what written
   gimmePerson