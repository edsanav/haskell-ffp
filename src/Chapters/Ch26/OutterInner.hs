module Chapters.Ch26.OutterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- the betten parenteshis part is one big monad in reality 
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap::ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap::ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap::() -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

--Prelude> readerUnwrap ()
--Right (Just 1)
-- Which is the same as applying return of each Monad instance individually
{-
instance Monad ((->) r) where
  return = const
instance Monad (Either e) where
  return = Right
instance Monad Maybe where
  return = Just
-}
--ghci> (const . Right . Just $ 1) ()
--Right (Just 1)

