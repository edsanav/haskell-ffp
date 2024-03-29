module Lookups where
import Control.Applicative
import Data.List (elemIndex)

-- Apply is like a functor combined with a semigroup
-- [(+1), (*2)] <*> [2, 4]
-- [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
-- [3,5,4,8]

-- Other example, first with a functor over the tuple function 
--(,) <$> [1, 2] <*> [3, 4]
--[(1, ), (2, )] <*> [3, 4]
--[(1,3),(1,4),(2,3),(2,4)]
-- or--
--liftA2 (,) [1, 2] [3, 4]
--[(1,3),(1,4),(2,3),(2,4)]

added::Maybe Integer
added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

y:: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled =  (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y2

xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed =  fmap sum $ (,) <$> x3 <*> y3