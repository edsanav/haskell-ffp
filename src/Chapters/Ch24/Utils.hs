module Chapters.Ch24.Utils(rp, parseNDigits, rpbs) where
import Text.Trifecta
import Data.ByteString (ByteString)


rp :: Parser a -> String -> Result a
rp f = parseString f mempty

rpbs::Parser a -> ByteString -> Result a
rpbs f = parseByteString f mempty


parseNDigits:: Int ->  Parser Int
parseNDigits n = read <$> count n digit