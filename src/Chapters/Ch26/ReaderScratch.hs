module Chapters.Ch26.ReaderScratch where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap.fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT (pure.pure x)
  (<*>) (ReaderT fmab) (ReaderT rma) = ReaderT $ \r -> (<*>) (fmab r) (rma r)
  -- or this without unpacking the function and just fmaping the ap
  -- in the end consider the function as any other structure as it was MaybeT
--  (<*>) (ReaderT fmab) (ReaderT rma) = ReaderT $ fmap (<*>) fmab <*> rma
1

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  
  (ReaderT rma) >>= f = ReaderT $ \r -> do 
    a <- rma r
    runReaderT (f a) r