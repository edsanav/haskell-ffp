module Laws where
import Test.QuickCheck


appIdentity::(Eq (f a), Applicative f) => f a -> Bool
appIdentity f1 = (pure id <*> f1) == f1


appCompose :: (Eq (f c), Applicative f) => f (b -> c) -> f (a -> b) -> f a -> Bool
appCompose fbc fab fa = (pure (.) <*> fbc <*> fab <*> fa) == (fbc <*> (fab <*> fa))

appCompose'::(Eq (f c), Applicative f)  => Fun b c -> Fun a b -> f a -> Bool
appCompose' (Fun _ bc) (Fun _ ab) fa = appCompose (pure bc) (pure ab) fa

runTest = do
  quickCheck (appIdentity::[Int] -> Bool)
  quickCheck (appCompose'::Fun Int Int -> Fun Int Int -> [Int] -> Bool)