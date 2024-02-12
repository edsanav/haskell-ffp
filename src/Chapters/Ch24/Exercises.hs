module Chapters.Ch24.Exercises where
import Control.Applicative

import Text.Trifecta
  
  
p :: Parser a -> String -> Result a
p f = parseString f mempty

parseDigit :: Parser Char
parseDigit = choice $ fmap char "0123456789"

base10Integer :: Parser Integer
base10Integer = option (1::Integer) ((-1::Integer) <$ char '-') >>= (\mult -> (*mult) <$> (read <$> some parseDigit)) 

base10Integer' :: Parser Integer
base10Integer' = do 
  mult  <- option (1::Integer) ((-1::Integer) <$ char '-')
  digis <- some parseDigit
  return $ mult * foldl (\b a -> b*10 + read [a]) 0 digis

main:: IO ()
main = do
  print $ p parseDigit "123"
  print "Done"