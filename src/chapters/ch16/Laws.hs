module Laws where

import Test.QuickCheck


-- Laws are
-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)

functorIdentity::(Eq (f b), Functor f) => f b -> Bool
functorIdentity f1 = fmap id f1 == id f1

functorCompose::(Eq (f c), Functor f)  => (a ->b ) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g (fmap f x))

functorCompose'::(Eq (f c), Functor f)  => (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
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

test2 :: (Int -> String) -> (String -> Bool) -> Identity Int -> Bool
test2 = functorCompose



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
--  quickCheck $ \(Fn f) (Fn g) x -> functorCompose:: 