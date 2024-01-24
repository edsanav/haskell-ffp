{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Chapters.Ch24.Marshalling where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
"whatisit": {"red": "intoothandclaw"}
}
|]

-- FromJSON instance accepts the Value type and ToJSON instance generates the Value Type

-- FromJSON
-- ByteString -> Value -> yourType
-- parse -> unmarshall

-- ToJSON
--yourType -> Value -> ByteString
-- marshall -> serialize

data TestData = TestData {section::Host, what::Color} deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) = (Red <$> v .: "red") <|> (Blue <$> v.: "blue") <|> (Yellow <$> v.: "yellow")
  parseJSON _ = fail "Expected an object for Host"

data NumberOrString = Numba Integer | Stringly Text deriving (Show, Eq)

instance FromJSON NumberOrString where
  parseJSON (Number i) = case floatingOrInteger i of 
                          (Left _) -> fail "Must be integral number"
                          (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringly s
  parseJSON _ = fail "NumberOrString must be number or string"

dec::LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec::LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main = do
  let blah :: Maybe TestData
      blah = decode sectionJson
  print blah
  print $ dec "blah"
  print $ eitherDec "blah"
  print $ dec "123"
  print $ dec "\"bla\""