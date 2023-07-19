module EitherMon where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
  
instance Monoid a => Applicative (Sum a) where
  pure = Second  
  (<*>) (First a) (First a2) = First (x <> x2)
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second b) = Second (f b)
  
instance Monoid a => Monad (Sum a) where
  return = pure
  (>>=) (First a) _ =  First a
  (>>=) (Second b) f =  f 1b
