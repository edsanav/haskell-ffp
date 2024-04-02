{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch25.Scotty where

import Web.Scotty

import Data.Monoid (mconcat)
import Control.Monad.Trans.Class

-- for reference
-- type ActionM = ActionT Text IO

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift (putStrLn "hello")
    
    html $ 
      mconcat ["<h1>Scotty, ",
            beam,
            " me up!</h1>"]