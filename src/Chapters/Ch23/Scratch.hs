module Scratch where

type Iso a b = (a->b, b->a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)


newtype State s a = State { runState :: s -> (a, s) }

-- :t State
-- State :: (s -> (a, s)) -> State s a
-- :t runState
-- runState :: State s a -> s -> (a, s)