module Chapters.Ch26.StateScratch where

import Data.Bifunctor (first)
import Control.Monad.IO.Class


class MonadTrans t where
  lift :: (Monad m) => m a -> t m a


newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

-- first applies function to first argument 
instance (Functor m) => Functor (StateT s m) where 
  fmap f (StateT sma) = StateT $ \s -> fmap (first f) (sma s)
                          

instance (Monad m) => Applicative (StateT s m) where 
  pure x = StateT (\s -> pure (x, s))
  
  (<*>) (StateT smfab) (StateT sma) = StateT (\s -> do
                                        (fab, s1) <- smfab s
                                        (a, s2) <- sma s1
                                        return (fab a, s2)
                                      ) 

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT (\s -> do
                (a, s1) <- sma s
                runStateT (f a) s1)

instance MonadTrans (StateT s) where 
  lift ma = StateT (\s -> do 
                    a <- ma
                    return (a, s))

-- Other example from: https://github.com/heitor-lassarote/haskell-programming-from-first-principles/blob/master/26%20-%20Monad%20transformers/StateT.hs
--instance MonadTrans (StateT s) where
--    lift mas = StateT $ \s -> (, s) <$> mas

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift.liftIO

