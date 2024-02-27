{-# LANGUAGE InstanceSigs #-}

module Chapters.Ch25.IdentityScratch where

newtype Identity a = Identity {runIdentity::a}

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

-- Example: You already have f and g: f is list g is Maybe, a is Int
-- Type constructors can take other type constuctors as arguments
--ghci> xs = [Just (1::Int), Nothing]
--ghci> Compose xs
--Compose {getCompose = [Just 1,Nothing]}
--ghci> :t Compose xs
--Compose xs :: Compose [] Maybe Int

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- Remember f and g need to be part of the structure we are lifting
-- Also, fmap is applied twice to reach the value inside the layered structure
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- closed under composition: composing two functors gives you another functo

instance (Applicative Identity) where
  pure:: a -> Identity a
  pure = Identity 
  
  (<*>)::Identity (a ->b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity a) = Identity (f a)
  
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure:: a -> Compose f g a
  pure x = Compose $ pure $ pure x

  (<*>)::Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose (fgab)) (Compose fga) = undefined
  
-- TODO: finish this 
-- let bla = pure (5+)::Compose [] Maybe (Int->Int)
-- let ble = pure (2)::Compose [] Maybe (Int)
