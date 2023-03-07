module Scratch where

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


check ::IO()
check = do
  quickCheck (monoidAssoc::MA)
  quickCheck (monoidRightIdentity::String->Bool)
  quickCheck (monoidLeftIdentity::String -> Bool)
  