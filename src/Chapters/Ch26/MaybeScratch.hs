module Chapters.Ch26.MaybeScratch where

import Chapters.Ch25.IdentityScratch (Identity)
import Data.Either (Either)

newtype MaybeT m a = MaybeT {runMaybeT:: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where 
  fmap f (MaybeT ma) = MaybeT $ (fmap.fmap) f ma
  
instance (Applicative m) => Applicative (MaybeT m) where 
  pure x = MaybeT (pure (pure x))
  
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fmap (<*>) fab <*> mma
  

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)] -> [ Maybe (Identity a) -> Maybe (Identity b) ]
second' = fmap (<*>)

final':: [ Maybe (Identity a)-> Maybe (Identity b) ]-> [Maybe (Identity a)]-> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
lmiApply f x = final' (second' (innerMost f)) x

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  
  (>>=)::MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do 
    v <- ma
    case v of 
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
 
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema )= EitherT $ (fmap.fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure ema = EitherT $ (pure.pure) ema
  
  (<*>) (EitherT emfab) (EitherT ema) = EitherT $ (fmap (<*>) emfab) <*> ema   
  
instance Monad m => Monad (EitherT e m) where 
  return = pure
  
  (EitherT ema) >>= f = EitherT $ do 
    v <- ema
    case v of 
      Left e -> return (Left e)
      Right a -> runEitherT $ f a

swapEitherT::(Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap sw ema
  where sw (Left e) = Right e
        sw (Right a) = Left a 

eitherT::(Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ema) = do
  v <- ema
  case v of
    Left a -> f a
    Right b -> g b 
