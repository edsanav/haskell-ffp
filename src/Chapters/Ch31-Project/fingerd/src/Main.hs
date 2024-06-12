{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import qualified Network.Socket as S
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ
import Data.ByteString.Char8 (putStrLn)

data User = User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
} deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) = toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
 (id INTEGER PRIMARY KEY AUTOINCREMENT,
 username TEXT UNIQUE,
 shell TEXT, homeDirectory TEXT,
 realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers:: Query
allUsers = "SELECT * FROM USERS"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

-- abour error handling: https://fpcomplete.com/blog/exceptions-best-practices-haskell/ super interesting!
instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser::Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase::IO()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  
  rows <- query_ conn allUsers
  mapM_ print (rows::[User])
  SQLite.close conn
  
  where meRow::UserRow
        meRow = (Null, "edsanav", "/bin/zsh", "/home/eduardo", "Eduardo Santmaria", "941342312")


formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
 [ "Login: ", e username, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t\t",
   "Shell: ", e shell,  "\n"
 ]
 where e = encodeUtf8


returnUsers::Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers

  let usernames = map username rows
      newLineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newLineSeparated)

returnUser:: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      Prelude.putStrLn ("Couldn't find matching user for username:" ++ show (T.strip username))
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  Prelude.putStrLn "Got connection, handle query"
  handleQuery dbConn soc
  S.close soc


main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags =[AI_PASSIVE]})) Nothing (Just "79")

  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  S.bind sock $ addrAddress serveraddr
  listen sock 1

  conn <- open "finger.db"
  handleQueries conn sock

  SQLite.close conn
  S.close sock

-- Further reading: https://www.fpcomplete.com/blog/async-exception-handling-haskell/