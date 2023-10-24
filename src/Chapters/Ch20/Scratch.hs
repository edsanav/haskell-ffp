module PartiallyApplied where

data MyIdentity a = MyIdentity a deriving (Show, Eq)

-- Careful with the order
-- foldr/fold the f tells you how to combine (monoid of the operation)
-- foldMap applies function and collapses according to Monoid instance of the result
-- foldMap can have a different instance to the one at the begining
instance Foldable MyIdentity where
  foldr f z (MyIdentity x) = f x z
  foldl f z (MyIdentity x) = f z x
  foldMap f (MyIdentity x) = f x

{-
Prelude> foldr (*) 1 (MyIdentity 5)
5
Prelude> foldl (*) 5 (MyIdentity 5)
25
Prelude> fm = foldMap (*5)
Prelude> type PI = Product Integer
Prelude> fm (MyIdentity 100) :: PI
Product {getProduct = 500} 
-}