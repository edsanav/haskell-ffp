module Scratch where
import Test.QuickCheck


appIdentity::(Eq (f a), Applicative f) => f a -> Bool
appIdentity f1 = (pure id <*> f1) == f1


appCompose :: (Eq (f c), Applicative f) => f (b -> c) -> f (a -> b) -> f a -> Bool
appCompose fbc fab fa = (pure (.) <*> fbc <*> fab <*> fa) == (fbc <*> (fab <*> fa))

appCompose'::(Eq (f c), Applicative f)  => Fun b c -> Fun a b -> f a -> Bool
appCompose' (Fun _ bc) (Fun _ ab) fa = appCompose (pure bc) (pure ab) fa


-- Homomorphism
hLeft = (pure (+1) <*> pure 1)::Maybe Int
hRight = pure ((+1) 1) :: Maybe Int


-- Interchange: u <*> pure y = pure ($ y) <*> u
-- $ changes the preference? it makes y to expect a function to which apply it:
-- Prelude> :t ('a')
-- ('a') :: Char
-- Prelude> :t ($ 'a')
-- ($ 'a') :: (Char -> b) -> b
-- so: These are the same
-- ($ 2)
-- \f -> f $ 2

runTest = do
  quickCheck (appIdentity::[Int] -> Bool)
  quickCheck (appCompose'::Fun Int Int -> Fun Int Int -> [Int] -> Bool)
  print (hLeft == hRight)
  print ((Just (+2) <*> pure 1) == (pure ($ 1) <*> Just (+2)))
