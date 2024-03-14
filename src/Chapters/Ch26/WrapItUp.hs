module Chapters.Ch26.WrapItUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = undefined
--embedded = ( MaybeT (ExceptT (ReaderT (const (Right (Just 1))))))
