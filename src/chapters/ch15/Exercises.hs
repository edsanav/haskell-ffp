module Exercises where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

semigroupAssoc::(Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance  Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) =  Identity $ (<>) x y
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary =  Identity <$> arbitrary 
 
type IdenAssoc a = Identity a -> Identity a -> Identity a ->  Bool

data Two a b = Two a b deriving (Eq, Show)
                       
instance  (Semigroup a, Semigroup b) => Semigroup (Two a b) where
 (<>) (Two x y) (Two x' y') =  Two (x <> x') ( y<> y')
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
 arbitrary = Two <$> arbitrary  <*> arbitrary
{- above is the same as the following but without syntatic sugar
 arbitryary = do
   a <- arbitrary
   b <- arbitrary
   return Two a b 
-}

type TwoAssoc a b = Two a b -> Two a b -> Two a b ->  Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)
  
instance Arbitrary BoolConj where 
  arbitrary = elements [BoolConj True, BoolConj False]
 
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)
  
instance Arbitrary BoolDisj where 
  arbitrary = elements [BoolDisj True, BoolDisj False]

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd a) _  = (Snd a)
  (<>) (Fst _) (Snd b) = (Snd b)
  (<>) (Fst _) (Fst b) = (Fst b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
 arbitrary =  frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc a b = Or a b -> Or a b -> Or a b ->  Bool



main :: IO()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdenAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String Ordering)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc String Ordering)
