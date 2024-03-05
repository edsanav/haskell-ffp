{-# LANGUAGE InstanceSigs #-}

module Chapters.Ch25.ExampleIdentity where

newtype Identity a = Identity {runIdentity::a} deriving (Eq, Show)
newtype IdentityT f a = IdentityT {runIdentityT:: f a} deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m)  => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity fa) (Identity a) = Identity (fa a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (<*>) (IdentityT fma) (IdentityT ma) = IdentityT (fma <*> ma)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure

  -- Note that the bind (>>=) in the definition is for the inner monad, not the same as the outside
  -- so that has type (>>=) m a -> (a -> m b) -> m b
  -- so we need to composef with runIdentityT because f is type (a -> IdentityT m b), so if applied directly
  -- it will produce (IdentityT m b) which is not the same as m and won't work
  -- (>>= merges structure of the same type after lifting, remember, it's like join composed with fmap1)
  -- generally speaking we are following this steps with the types:
    --  m (T m b)
    --  -> m (m b)
    --  -> m b
    --  -> T m b
  (>>=):: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = -- IdentityT $ ma >>= runIdentityT.f
