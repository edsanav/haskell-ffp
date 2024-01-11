module Chapters.Ch24.LearnParsers where

import Text.Trifecta
import Control.Applicative

-- For reference:
-- ghci> runParser (char 'a') mempty "a"
-- Success 'a'


stop:: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneEOF:: Parser Char
oneEOF = char '1' <* eof


one' :: Parser b
one' = one >> stop

oneTwo:: Parser Char
oneTwo = char '1' >> char '2'

oneTwoEOF:: Parser Char
oneTwoEOF =  char '1' >> char '2' <* eof

oneTwo':: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print  $ parseString p mempty "123"
  
pNL:: String -> IO ()
pNL s = putStrLn ('\n' : s)

p123:: Parser String
p123 = (string "123" <|> string "12" <|> string "1") <* eof

pString::String -> Parser String 
pString = traverse char

main::IO()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEOF:"
  testParse oneEOF
  pNL "oneTwoEOF:"
  testParse oneTwoEOF
  pNL "pString:"
  print $ runParser (pString "bla") mempty "a"
  pNL "pString:"
  print $ runParser (pString "bla") mempty "bla"