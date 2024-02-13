module Chapters.Ch24.Utils(rp, parseNDigits) where
import Text.Trifecta

rp :: Parser a -> String -> Result a
rp f = parseString f mempty

parseNDigits:: Int ->  Parser Int
parseNDigits n = read <$> count n digit