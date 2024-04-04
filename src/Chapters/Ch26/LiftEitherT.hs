module Chapters.Ch25.LiftEitherT where

import Control.Monad 

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

newtype EitherT e m a = EitherT { runExceptT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
  
