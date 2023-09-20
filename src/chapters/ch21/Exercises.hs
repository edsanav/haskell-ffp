module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Identity a =  Identity a deriving (Eq, Ord, Show)

instance (Arbitrary a ) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

------------------------------------------------------------------------------
-- check differences from previous chapter (here data constructor corresponds to the first arg of type constructor)
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

------------------------------------------------------------------------------------------
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (Optional a) where
   arbitrary = do
      a <- arbitrary
      elements [Nada, Yep a]
--  arbitrary = arbitrary >>= (\x -> elements [Nada, Yep x])

instance (Eq a) => EqProp (Optional a ) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, Cons a b]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  -- Cons <$> f x has type f (List b -> List b) thats why we combine with "traverse f xs" through <*>  
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  


main = do
  quickBatch $ traversable (undefined:: Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined:: Constant Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined:: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined:: List (Int, Int, [Int]))