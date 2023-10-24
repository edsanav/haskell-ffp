module MainCiph where

import VigCipher(vigne, unvigne, caesar, uncaesar)

import System.IO
import Data.Char (toLower)


askForKey::IO String
askForKey  = do
  putStrLn "Please enter the word to use as key"
  code  <- getLine
  return code

askForInt:: IO Int
askForInt = do
  putStrLn "Please enter the Caesar number to use as key"
  n <- getLine
  i <- readIO n
  return i


runEncodeV:: IO()
runEncodeV = do
    code <- askForKey
    putStrLn "Please enter the phrase to cipher"
    word <- getLine
    putStrLn $ "Your coded phrase is: "++ (vigne word code)

runDecodeV:: IO()
runDecodeV = do
    code <- askForKey
    putStrLn "Please enter the phrase to uncipher"
    word <- getLine
    putStrLn $ "Your decoded phrase is: "++ (unvigne word code)
    
runEncodeC::IO()
runEncodeC = do
    i <- askForInt
    putStrLn "Please enter the phrase to cipher"
    word <- getLine
    putStrLn $ "Your coded phrase is: "++ (caesar word i)

runDecodeC::IO()
runDecodeC = do
    i <- askForInt
    putStrLn "Please enter the phrase to cipher"
    word <- getLine
    putStrLn $ "Your decoded phrase is: "++ (uncaesar word i)


main :: IO()
main = do
  hSetBuffering stdout NoBuffering -- If removed, it doesn't print what written
  putStrLn "Please select cipher type (caesar/vig)?"
  ciphType <- getLine
  putStrLn "Please select what do you want to do (encode/decode)?"
  action <- getLine
  let lType = map toLower ciphType
  let lAction = map toLower action
  case (lType, lAction) of 
    ("caesar", "encode") -> runEncodeC
    ("caesar", "decode") -> runDecodeC
    ("vig", "encode") -> runEncodeV
    ("vig", "decode") -> runDecodeV
    (_, _) -> putStrLn ("Some of the options are not valid: " ++ ciphType ++ ", "++action)