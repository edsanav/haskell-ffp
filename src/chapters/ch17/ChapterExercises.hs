module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a = Pair a a deriving (Show, Eq)


instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair fa fa2) (Pair a a2) = Pair (fa a) (fa2 a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <-arbitrary
    a2 <- arbitrary
    return (Pair a a2)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq
-----------------------------------------------------
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance  Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a1 fb) (Two a2 b) = Two (a1 <> a2) (fb b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq
-----------------------------------------------------
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c ) = Three a b (f c)

instance  (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a1 b1 fc) (Three a2 b2 c) = Three (a1 <> a2) (b1 <> b2) (fc c)


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c ) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
-----------------------------------------------------
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b2 ) = Three' a (f b) (f b2)

instance  (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a1 fb fb') (Three' a2 b b') = Three' (a1 <> a2) (fb b) (fb' b')


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b ) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
-----------------------------------------------------
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance  (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a1 b1 c1 fd) (Four a2 b2 c2 d) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (fd d)


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
-----------------------------------------------------
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a2 a3 b) = Four' a a2 a3 (f b)

instance  (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a1 a1' a1'' fb) (Four' a2 a2' a2'' b) = Four' (a1 <> a2) (a1' <> a2') (a1'' <> a2'') (fb b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
-------------------------------------------------------

main :: IO ()
main = do
  quickBatch $ functor (undefined::Pair (Int, Double, String))
  quickBatch $ applicative (undefined::Pair (Int, Double, String))
  quickBatch $ functor (undefined::Two Integer (Int, Double, String))
  quickBatch $ applicative (undefined::Two String (Int, Double, String))
  quickBatch $ functor (undefined::Three Integer Bool (Int, Double, String))
  quickBatch $ applicative (undefined::Three Ordering String (Int, Double, String))
  quickBatch $ functor (undefined::Three' Integer (Int, Double, String))
  quickBatch $ applicative (undefined::Three' Ordering (Int, Double, String))
  quickBatch $ functor (undefined::Four Integer Bool Ordering (Int, Double, String))
  quickBatch $ applicative (undefined::Four String String Ordering (Int, Double, String))
  quickBatch $ functor (undefined::Four' Integer (Int, Double, String))
  quickBatch $ applicative (undefined::Four' Ordering (Int, Double, String))