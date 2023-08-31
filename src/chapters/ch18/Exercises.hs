module Exercises where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join, liftM2, ap, mapM)


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

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) ffa = ffa a

instance (Arbitrary a ) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq


data List a = Nil | Cons a (List a)  deriving (Eq, Show)

instance Functor List where
  fmap _  Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
  
instance Applicative (List) where
  pure x = Cons x Nil
  (<*>) Nil Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f xsf) fa@(Cons a xs) = Cons (f a) (fmap f xs) `append` (xsf <*> fa)

instance Monad (List) where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a xs) f = f a `append` (xs >>= f)  

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

    
instance (Arbitrary a ) => Arbitrary (List a) where
  arbitrary = do 
     a <- arbitrary
     b <- arbitrary
     elements [Nil, Cons a b]
-- same as:
--instance (Arbitrary a ) => Arbitrary (List a) where
--  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- joing it's like j x = x >> id
j :: Monad m => m (m a) -> m a
j  = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)
  
l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' = liftM2 

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = do 
  x <- m
  f <- mf
  return (f x)
 
a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
--meh' (x:xs) f = f x >>= \b -> meh xs f >>= \bs -> return $ b : bs
meh (x:xs) f = do
  b <- f x
  bs <- meh xs f
  return (b:bs)

-- un traverse como un castillo
meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' xs f = mapM f xs

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id


main = do
  quickBatch $ monad (undefined:: Nope (Int, String, Int))
  quickBatch $ monad (undefined:: Identity (Int, String, Int))
  quickBatch $ monad (undefined:: List (Int, String, Int))