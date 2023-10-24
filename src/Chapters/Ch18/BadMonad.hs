module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f = let CountMe _ b = f a in CountMe (n + 1) b
 
instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq
  
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  
-- This outputs:
{-
applicative:
  identity:     +++ OK, passed 500 tests.
  composition:  +++ OK, passed 500 tests.
  homomorphism: +++ OK, passed 500 tests.
  interchange:  +++ OK, passed 500 tests.
  functor:      *** Failed! Falsifiable (after 1 test):  
<function>
CountMe 0 0

monad laws:
  left  identity: *** Failed! Falsifiable (after 1 test):  
<function>
0
  right identity: *** Failed! Falsifiable (after 1 test):  
CountMe 0 0
  associativity:  *** Failed! Falsifiable (after 1 test):  
CountMe 0 0

-}
