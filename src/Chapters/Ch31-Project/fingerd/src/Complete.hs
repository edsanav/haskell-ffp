{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import Data.Text.Encoding ( decodeUtf8, encodeUtf8, decodeUtf8 )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Control.Monad (forever)
import Network.Socket hiding (close, recv)
import qualified Network.Socket as S
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)
import GHC.Generics

import Data.Aeson

-- https://github.com/crabmusket/haskell-simple-concurrency/blob/master/src/tutorial.md
-- example: https://smunix.github.io/chimera.labs.oreilly.com/books/1230000000929/ch12.html
-- TODO socket for reading, socket for writing (this one forked)

data User = User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
} deriving (Generic, Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) = toRow (id_, username, shell, homeDir, realName, phone)

instance FromJSON User

instance ToJSON User


data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)
instance Exception DuplicateData

allUsers:: Query
allUsers = "SELECT * FROM USERS"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

updateUserQuery :: Query
updateUserQuery = "UPDATE users SET username = ?, shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE id = ?"

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
 [ "Login: ", e username, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t\t",
   "Shell: ", e shell,  "\n"
 ]
 where e = encodeUtf8

getUser::Connection -> Text -> IO (Maybe User)
getUser conn username = do
 results <- query conn getUserQuery (Only username)
 case results of
   [] -> return Nothing
   [user] -> return $ Just user
   _ -> throwIO DuplicateData

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

-- UPDATE {"userId":0, "username":"pepe", "shell":"/bin/bash","homeDirectory":"/home/nuNEW","realName":"Pepe","phone":"+35"}
modifyUser::Connection -> User -> IO ()
modifyUser conn User {..} = do
  maybeUser <- getUser conn $ T.strip username
  case maybeUser of
    Nothing -> do
      Prelude.putStrLn $ "Couldn't find matching user for username:" ++ show (T.strip username)
    Just (User theId _ _ _ _ _) -> do
      _ <- execute conn updateUserQuery (username, shell, homeDirectory, realName, phone, theId)
      return ()

-- INSERT {"userId":0, "username":"pepe", "shell":"/bin/bash","homeDirectory":"/home/nu","realName":"Pepe","phone":"+35"}
insertUser::Connection -> User -> IO ()
insertUser conn User {..} = do
  _ <- execute conn insertUserQuery (Database.SQLite.Simple.Types.Null, username, shell, homeDirectory, realName, phone)
  return ()


handleQuery::Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  -- threadDelay 10000000 -- sleep for 10 seconds, simulate slow operation
  msg <- recv soc 1024
  let cleanMsg = T.strip $ decodeUtf8 msg
  case cleanMsg of
    "" -> returnUsers dbConn soc
    name -> returnUser dbConn soc name

handleWrite::Connection -> Socket -> IO ()
handleWrite dbConn soc = do
  msg <- recv soc 1024
  let cleanMsg = T.strip $ decodeUtf8 msg
  case T.words cleanMsg of
    "INSERT":xs -> case deserializeUser (BS.fromStrict $ encodeUtf8  $ T.concat xs) of
                  Nothing -> Prelude.putStrLn $ "Couldn't parse" ++ T.unpack (T.concat xs)
                  (Just user) -> insertUser dbConn user
    "UPDATE":xs -> case deserializeUser (BS.fromStrict $ encodeUtf8  $ T.concat xs)  of
                   Nothing -> Prelude.putStrLn $ "Couldn't parse" ++ T.unpack (T.concat xs)
                   (Just user) -> modifyUser dbConn user
    _ -> sendAll soc $ encodeUtf8 $ "Invalid write command: " <> cleanMsg <> "\n"
  where deserializeUser jUser = decode jUser

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  Prelude.putStrLn "Got connection, handle query"
  (SockAddrInet port _)  <- getSocketName sock
  case port of
    79 -> handleQuery dbConn soc
    _ -> handleWrite dbConn soc
  S.close soc


getSocket::String -> IO Socket
getSocket portNumb = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags =[AI_PASSIVE]})) Nothing (Just portNumb)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  S.bind sock $ addrAddress serveraddr
  listen sock 1
  return sock

main :: IO ()
main = withSocketsDo $ do
  querySock <- getSocket "79"
  insertSock <- getSocket "81"

  conn <- open "finger.db"

  _ <- forkIO (handleQueries conn querySock)
  _ <- handleQueries conn insertSock

  S.close querySock
  S.close insertSock