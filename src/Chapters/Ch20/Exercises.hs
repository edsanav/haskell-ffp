module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Constant a b = Constant b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq b) => EqProp (Constant a b) where
  (=-=) = eq
  
instance Foldable (Constant a) where
  -- not sure which one
--  foldr _ b _= b -- this doesn't pass certain tests with checkers
  foldr f b (Constant x) = f x b

data Two a b = Two a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
 arbitrary = Two <$> arbitrary <*> arbitrary
  
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Foldable (Two a) where 
  foldMap f (Two _ y) = f y
  foldr f b (Two _ y) = f y b -- not necessary really if defining foldMap
------------

data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
 arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
  
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
  
instance Foldable (Three a b) where 
  foldMap f (Three _ _ z) = f z
------
data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
 arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
  
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
  
instance Foldable (Three' a) where 
  foldMap f (Three' _ y1 y2) = f y1 <> f y2
---
data Four' a b = Four' a b b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
 arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary
  
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
  
instance Foldable (Four' a) where 
  foldMap f (Four' _ y1 y2 y3) = f y1 <> f y2 <> f y3
  
-- Convert f to something we can apply inside the provided foldable and then just lift result (or mempty)
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

main :: IO ()
main = do
  quickBatch $ foldable (undefined::Constant Bool (String, Bool, String, Float, Char))
  quickBatch $ foldable (undefined::Two Bool (String, Bool, String, Float, Char))
  quickBatch $ foldable (undefined::Three Bool String (String, Bool, String, Float, Char))
  quickBatch $ foldable (undefined::Three' Bool (String, Bool, String, Float, Char))
  quickBatch $ foldable (undefined::Four' Bool (String, Bool, String, Float, Char))
