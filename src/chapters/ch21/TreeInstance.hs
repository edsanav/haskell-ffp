module TreeInstance where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)
  

instance (Arbitrary a ) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    elements [Empty, Leaf a, Node l a r]
    
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq
    
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$> traverse f left <*> f a <*> traverse f right
  
  
main = do
  quickBatch $ traversable (undefined:: Tree (Int, Int, [Int]))