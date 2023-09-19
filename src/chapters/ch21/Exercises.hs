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

  


main = do
  quickBatch $ traversable (undefined:: Identity (Int, Int, [Int]))