{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch25.Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types(ActionT(..))
import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Control.Monad.Trans.Except
import Control.Monad 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict hiding (get)

-- for reference
-- newtype ActionT e (m :: * -> *) a
--  = ActionT {runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a}
   
-- type ActionM = ActionT Text IO

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (lift) (putStrLn "hello")
    
    html $ 
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]

  -- equivalent. concretizing..
  get "/:word2" $ do
    beam <- param "word2"
    (ActionT . lift . lift . lift) (putStrLn "hello")

    html $
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]

  -- equivalent, concretizing
  get "/:word3" $ do
    beam <- param "word3"
    (ActionT . (ExceptT . liftM Right) . lift . lift) (putStrLn "hello")

    html $
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]

  -- equivalent, concretizing
  get "/:word4" $ do
    beam <- param "word4"
--    (ActionT . (ExceptT . liftM Right) . (\m -> ReaderT (const m)) . lift) (putStrLn "hello")
    (ActionT . (ExceptT . fmap Right) . (ReaderT . const) . lift) (putStrLn "hello")

    html $
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]

  -- equivalent, concretizing
  get "/:word5" $ do
    beam <- param "word5"
    (ActionT . (ExceptT . liftM Right) . (ReaderT . const) . \m -> StateT (\s ->do
                                                                                   a <- m
                                                                                   return (a, s))) (putStrLn "hello")

    html $
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]
  
  -- liftIO
  get "/liftio/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    
    html $
      mconcat ["<h1>Scotty, ",
                          beam,
                          " me up!</h1>"]