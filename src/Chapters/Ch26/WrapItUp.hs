module Chapters.Ch26.WrapItUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT (ExceptT (ReaderT (const (return $ Right (Just 1)))))

-- It's a matter of adding layers.
-- First what we want to wrap
-- IO (Either (String (Maybe (Int)))
inner::() -> IO(Either String (Maybe Int))
inner = const $ return (Right (Just 1))

-- then we add first layer of transformers, reader
rd :: ReaderT () IO(Either String (Maybe Int))
rd = ReaderT inner

-- then exceptT, note types are "moving" from the inner wrapped to the transformers
exc :: ExceptT String (ReaderT () IO) (Maybe Int)
exc = ExceptT rd

-- then the maybe 
mb:: MaybeT (ExceptT String (ReaderT () IO)) Int
mb = MaybeT exc