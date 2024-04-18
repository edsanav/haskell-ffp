{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch26.ScottyExercise where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config {
  -- that's one, one click!
  -- two... two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
  counts :: IORef (M.Map Text Integer)
  , prefix :: Text
}
-- :t prefix
--ghci> :t prefix
--prefix :: Config -> Text

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, newMap M.! k)
  where newMap = M.insertWith (+) k 1 m




ex1::ReaderT Config IO Text
ex1 = ReaderT $ return.prefix

ex2:: ReaderT Config IO (M.Map Text Integer)
ex2 = ReaderT $ readIORef . counts

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key" :: Handler Text
--    pref <- (lift $ ReaderT $ return.prefix) :: Handler Text
    pref <- lift $ asks prefix
    let key' = mappend pref unprefixed
    counter <- lift $ ReaderT $ readIORef . counts
    counterRef <- lift $ ReaderT $ return . counts
    let bla = atomicModifyIORef counterRef (bumpBoomp key')
    newInteger <- lift $ ReaderT $ const bla  -- not sure about this
    html $
      mconcat [ "<h1>Success! Count was: "
      , TL.pack $ show newInteger
      , "</h1>"
      ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  countR <- newIORef M.empty
  let config = Config countR (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app