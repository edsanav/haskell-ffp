module Exercises where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

semigroupAssoc::(Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

mli::(Eq m, Monoid m) => m -> Bool
mli a = (mempty <> a) == a

mlr::(Eq m, Monoid m) => m -> Bool
mlr a = (a <> mempty) == a

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance  Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) =  Identity $ (<>) x y

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary =  Identity <$> arbitrary

type IdenAssoc a = Identity a -> Identity a -> Identity a ->  Bool

data Two a b = Two a b deriving (Eq, Show)

instance  (Semigroup a, Semigroup b) => Semigroup (Two a b) where
 (<>) (Two x y) (Two x' y') =  Two (x <> x') ( y<> y')
 
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty 

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
  
instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || 

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd a) _  = Snd a
  (<>) (Fst _) (Snd b) = Snd b
  (<>) (Fst _) (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
 arbitrary =  frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc a b = Or a b -> Or a b -> Or a b ->  Bool

newtype Combine a b = Combine {uncombine ::a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

-- Aa copied from https://stackoverflow.com/a/41353825
funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) = property $ \a -> f a === g a

combineAssoc :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))
--

newtype Comp a = Comp {umComp::a->a}

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f<>g)

compEq :: (Show a, Arbitrary a, Eq a) =>  Comp a -> Comp a -> Property
compEq (Comp f) (Comp g) = property $ \x -> f x == g x

compAssoc ::(Arbitrary a, Eq a, Show a, Semigroup a) => Comp a -> Comp a -> Comp a -> Property
compAssoc cf cg ch = compEq ((cf <> cg) <> ch) (cf <> (cg <> ch))

data Validation a b = Fail a | Succ b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Succ b) _ = Succ b
  (<>) (Fail a) (Succ b) = Succ b
  (<>) (Fail a1) (Fail a2) = Fail (a1 <> a2)

main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdenAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String Ordering)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc String Ordering)
  -- Fn is a modifier for testing function: since it's a modifier you pass it before the propertie
  -- It is also a pattern, that's the reason of the syntax \(Fn f) (Fn g) (Fn h) ->
  -- https://stackoverflow.com/a/27074021 
  quickCheck $ \(Fn f) (Fn g) (Fn h) -> (combineAssoc :: CombineAssoc Int Ordering) (Combine f) (Combine g) (Combine h)
  quickCheck $ \(Fn f) (Fn g) (Fn h) -> (compAssoc :: Comp Ordering -> Comp Ordering -> Comp Ordering -> Property) (Comp f) (Comp g) (Comp h)

  -- Monoids
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)
  quickCheck (mli :: Identity Ordering -> Bool)
  quickCheck (mlr :: Identity Ordering -> Bool)
  quickCheck (mli :: Two String Ordering -> Bool)
  quickCheck (mlr :: Two String Ordering -> Bool)




main2 = do
  let failure ::String -> Validation String Int
      failure = Fail
      success :: Int -> Validation String Int
      success = Succ
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2