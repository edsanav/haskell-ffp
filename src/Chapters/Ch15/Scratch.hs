module PartiallyApplied where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool
type MA = S -> S -> S -> B

monoidLeftIdentity::(Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity::(Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]
  
instance Semigroup Bull where
  (<>) _ _ = Fools
  
instance Monoid Bull where
  mempty = Fools
  
type BullMappend = Bull -> Bull -> Bull -> Bool

  
check ::IO()
check = do
  quickCheck (monoidAssoc::MA)
  quickCheck (monoidRightIdentity::String->Bool)
  quickCheck (monoidLeftIdentity::String -> Bool)
  quickCheck (monoidAssoc::BullMappend)
  quickCheck (monoidRightIdentity::Bull->Bool)
  quickCheck (monoidLeftIdentity::Bull -> Bool)
  