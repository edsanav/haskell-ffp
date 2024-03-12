module Chapters.Ch26.StateScratch where

import Data.Bifunctor (first)
  
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

-- first applies function to first argument 
instance (Functor m) => Functor (StateT s m) where 
  fmap f (StateT sma) = StateT $ \s -> fmap (first f) (sma s)
                          

instance (Monad m) => Applicative (StateT s m) where 
  pure x = StateT (\s -> pure (x, s))
  
  (<*>) (StateT smfab) (StateT sma) = undefined