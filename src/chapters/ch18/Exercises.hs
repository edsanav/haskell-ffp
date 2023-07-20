module Exercises where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
  
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
  
instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg
  
instance Arbitrary (Nope a) where
  arbitrary = do
     return NopeDotJpg
  
instance EqProp (Nope a) where
  (=-=) = eq
  
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap _ (PRight b) = PRight b

instance Monoid b => Applicative (BahEither b) where
  pure = PLeft  
  (<*>) (PRight b) (PRight a2) = PRight (b <> a2)
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft f) (PLeft a) = PLeft (f a)

instance Monoid b => Monad (BahEither b) where
  return = pure
  (>>=) (PRight b) _ =  PRight b
  (>>=) (PLeft a) f =  f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [PRight b, PLeft a]

instance (Eq b, Eq a) => EqProp (BahEither b a) where
  (=-=) = eq



main = do
  quickBatch $ monad (undefined:: Nope (Int, String, Int))