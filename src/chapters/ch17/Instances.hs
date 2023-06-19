module Instances where
  
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
  
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)
  
newtype Constant a b = Constant { getConstant:: a } deriving (Eq, Ord, Show)

type C = Constant

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
  
instance Monoid a =>  Applicative (Constant a) where 
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant g) = Constant (f <> g)
 
