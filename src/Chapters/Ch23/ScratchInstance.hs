{-# LANGUAGE InstanceSigs #-}

module Chapters.Ch23.ScratchInstance where

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum {getSum :: a}

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- Moi instead of State to avoid clashes
newtype Moi s a = Moi {runMoi :: s -> (a, s)}

-- :t Moi
-- Moi :: (s -> (a, s)) -> Moi s a
-- :t runState
-- runState :: Moi s a -> s -> (a, s)

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (a, s1) = g s
     in (f a, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (a, s1) = g s
        (f1, s2) = f s1
     in (f1 a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, s1) = f s
        m2 = g a
     in runMoi m2 s1
