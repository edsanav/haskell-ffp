{-# LANGUAGE OverloadedStrings #-}

module Chapters.Ch25.Scotty2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

-- for reference
-- newtype ActionT e (m :: * -> *) a
--  = ActionT {runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a}
   
-- type ActionM = ActionT Text IO -- e is Text, m (monad) is IO, pending a to define 


param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k) (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main = scotty 3000 $ do
  get "/:num/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)

    html $
      mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  
