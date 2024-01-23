{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Chapters.Ch24.Marshalling where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Text.RawString.QQ

sectionJson :: BS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
"whatisit": {"red": "intoothandclaw"}
}
|]

main = do
  let blah :: Maybe Value 
      blah = decodeStrict sectionJson
  print blah