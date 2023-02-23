module Misc where
import Test.QuickCheck
import Data.List (sort)

half::(Fractional a) => a -> a
half x = x / 2

halfIdentity::(Fractional a) => a -> a
halfIdentity = (*2) . half

-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_half::(Fractional a, Eq a) => a -> Bool
prop_half n = halfIdentity n == n

prop_ordered::(Ord a) => [a] ->Bool
prop_ordered = listOrdered . sort

prop_plusAssociative::(Num a, Eq a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative::(Num a, Eq a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

prop_prodAssociative::(Num a, Eq a) => a -> a -> a -> Bool
prop_prodAssociative x y z = x * (y * z) == (x * y) * z

prop_prodCommutative::(Num a, Eq a) => a -> a -> Bool
prop_prodCommutative x y = x * y == y * x

-- TODO test with positives?
--prop_thereAndBackAgain :: Property
--prop_thereAndBackAgain = forAll charGen
--  (\c -> (charToMorse c
--    >>= morseToChar) == Just c)
    
prop_Quot1::(Integral a) => a -> a -> Bool
prop_Quot1 x y = quot x y * y + rem x y == x


main :: IO ()
main = do
  quickCheck (prop_half::Float -> Bool)
  quickCheck (prop_half::Rational -> Bool)
  quickCheck (prop_ordered::[Int] -> Bool)
  quickCheck (prop_ordered::[Char] -> Bool)
  quickCheck (prop_ordered::[String] -> Bool)
  quickCheck (prop_plusAssociative:: Int -> Int -> Int -> Bool)
  quickCheck (prop_plusCommutative:: Int -> Int -> Bool)
  quickCheck (prop_prodAssociative:: Int -> Int -> Int -> Bool)
  quickCheck (prop_prodCommutative:: Int -> Int -> Bool)
  quickCheck (prop_Quot1::Int->Int->Bool)
