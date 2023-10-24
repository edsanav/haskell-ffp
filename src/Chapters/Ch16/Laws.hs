{-# LANGUAGE FlexibleInstances #-}
-- this above is for the Flip f a b

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

-- consider anything except last parameter as part of the type (so f applies to the last type paramter)

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


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a) 

instance (Arbitrary a ) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (1, Yeppers <$> arbitrary)]
  
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
 
instance (Arbitrary a, Arbitrary b ) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (1, Second <$> arbitrary)]

data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance Functor (Quant a) where 
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [return Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary
  
data Flip f a b = Flip (f a b) deriving (Eq, Show)

instance Functor (Flip K a) where 
  fmap _ (Flip (K a)) = Flip $ K a 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    x <- arbitrary
    return (Flip $ K x)
    
newtype EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
  
instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

newtype LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

-- no quicktested
instance (Functor f') => Functor (LiftItOut f') where
  fmap f (LiftItOut ga) =  LiftItOut (fmap f ga)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f', Functor g') => Functor (Parappa f' g') where
  fmap f (DaWrappa ga ha) = DaWrappa (fmap f ga) (fmap f ha)

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g') => Functor (IgnoreOne f' g' a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g' => Functor (Notorious g' o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
  
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gla1 gla2 gla3) = MoreGoats (fmap f gla1) (fmap f gla2) (fmap f gla3)

data TalkToMe a = Halt | Print String a | Read (String -> a) 

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a) 
  fmap f (Read g') = Read (f . g')
  

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
  quickCheck (functorIdentity:: Possibly Int -> Bool)
  quickCheck (functorCompose':: Fun Int String -> Fun String Int -> Possibly Int -> Bool )
  quickCheck (functorIdentity:: Sum Int Ordering -> Bool)
  quickCheck (functorCompose':: Fun Ordering Int -> Fun Int String -> Sum Int Ordering -> Bool )
  quickCheck (functorIdentity:: Quant Int Ordering -> Bool)
  quickCheck (functorCompose':: Fun Int String -> Fun String Ordering -> Quant Bool Int -> Bool )
  quickCheck (functorIdentity:: K Int Ordering -> Bool)
  quickCheck (functorCompose':: Fun Int String -> Fun String Ordering -> K Bool Int -> Bool )
  quickCheck (functorIdentity:: Flip K Int Ordering -> Bool)
  quickCheck (functorCompose':: Fun Int String -> Fun String Ordering -> Flip K Bool Int -> Bool )
  quickCheck (functorIdentity:: EvilGoateeConst String Ordering-> Bool)
  quickCheck (functorCompose':: Fun Ordering Int -> Fun Int Int -> EvilGoateeConst Bool Ordering -> Bool)
--  quickCheck $ \(Fn f) (Fn g) x -> functorCompose:: 