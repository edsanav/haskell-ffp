{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (forever)
import Network.Socket hiding (close, recv)
import qualified Network.Socket as S
import Network.Socket.ByteString (recv)

-- https://github.com/crabmusket/haskell-simple-concurrency/blob/master/src/tutorial.md
-- example: https://smunix.github.io/chimera.labs.oreilly.com/books/1230000000929/ch12.html
-- TODO socket for reading, socket for writing (this one forked)

handleQuery::Socket -> IO ()
handleQuery soc = do
  threadDelay 10000000 -- sleep for 10 seconds, simulate slow operation
  msg <- recv soc 1024
  let cleanMsg = T.strip $ decodeUtf8 msg
  case cleanMsg of
    -- TODO proper parsing here
    "GET ALL" -> Prelude.putStrLn "GET ALL command received with"
    "GET"-> Prelude.putStrLn "GET COMMAND RECEIVED"
    "INSERT"  -> Prelude.putStrLn "INSERT command received with"
    "UPDATE" -> Prelude.putStrLn "UPDATE COMMAND RECEIVED"
    _ -> Prelude.putStrLn "No instruction provided"

handleQueries :: Socket -> IO ()
handleQueries sock = forever $ do
  (soc, _) <- accept sock
  Prelude.putStrLn "Got connection, handle query"
  handleQuery soc
  S.close soc


main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags =[AI_PASSIVE]})) Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  S.bind sock $ addrAddress serveraddr
  listen sock 1

  handleQueries sock

  S.close sock