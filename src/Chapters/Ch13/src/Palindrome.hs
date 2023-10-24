module Palindrome (palindrome) where
import Control.Monad
import System.Exit (exitSuccess) 
import Data.Char (toLower)

cleanSentence::String -> String
cleanSentence = filter (flip elem ['a'..'z']) .(map toLower)

palindrome :: IO()
palindrome = forever $ do
  line1 <- fmap cleanSentence getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome"
    False -> putStrLn "Nope!" >>= return exitSuccess 


