module MyValidation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

data Validation e a = MyFailure e | MySuccess a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (MyFailure e) = MyFailure e
  fmap f (MySuccess a) = MySuccess (f a)
    
instance Monoid e => Applicative (Validation e) where
  pure = MySuccess
  (<*>) (MySuccess f) (MySuccess a) = MySuccess (f a)
  (<*>) (MyFailure e) (MySuccess _) = MyFailure e
  (<*>) (MySuccess _) (MyFailure e) = MyFailure e
  (<*>) (MyFailure e1) (MyFailure e2) = MyFailure (e1 <> e2)
  

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [MyFailure e, MySuccess a]
     

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (undefined::Validation String (Int, Double, String))
