module Laws where

import Test.QuickCheck


-- Laws are
-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)

functorIdentity::(Eq (f b), Functor f) => f b -> Bool
functorIdentity f1 = fmap id f1 == id f1

functorCompose::(Eq (f c), Functor f)  => (a ->b ) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g (fmap f x))

functorCompose'::(Eq (f c), Functor f)  => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = functorCompose f g x

{-
review this
https://github.com/yamad/haskellbook-hpfp/blob/master/Ex16.hs

functorCompose':: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)
-}


test1 :: [Int] -> Bool
test1 x = functorIdentity x

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
  
data Two a b = Two a b  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary
  
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
  
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where 
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
  
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
 
data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where 
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

{-
1. newtype Identity a = Identity a
2. data Pair a = Pair a a
3. data Two a b = Two a b
4. data Three a b c = Three a b c
5. data Three' a b = Three' a b b
6. data Four a b c d = Four a b c d
7. data Four' a b = Four' a a a b
8. Can you implement one for this type? Why? Why not?
data Trivial = Trivial
-}

runTest = do
  quickCheck test1
  quickCheck (functorIdentity:: Identity Int -> Bool)
  quickCheck (functorCompose'::Fun Int Int -> Fun Int Int -> Identity Int -> Bool)
  quickCheck (functorIdentity:: Pair Int -> Bool)
  quickCheck (functorCompose':: Fun Int Int -> Fun Int Int -> Pair Int -> Bool)
  quickCheck (functorIdentity:: Two String Int -> Bool)
  quickCheck (functorCompose':: Fun Int Int -> Fun Int Int -> Two String Int -> Bool)
  quickCheck (functorIdentity:: Three String Int String -> Bool)
  quickCheck (functorCompose':: Fun Bool Int -> Fun Int Int -> Three String Int Bool -> Bool)
  quickCheck (functorIdentity:: Three' String Int -> Bool)
  quickCheck (functorCompose':: Fun Bool Int -> Fun Int Int -> Three' String Bool -> Bool)
  quickCheck (functorIdentity:: Four String Int String Ordering-> Bool)
  quickCheck (functorCompose':: Fun Ordering Int -> Fun Int Int -> Four String Int Bool Ordering -> Bool)
  quickCheck (functorIdentity:: Four' String Int -> Bool)
  quickCheck (functorCompose':: Fun Ordering Int -> Fun Int Int -> Four' String Ordering -> Bool)
--  quickCheck $ \(Fn f) (Fn g) x -> functorCompose:: 