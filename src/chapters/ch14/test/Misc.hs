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


prop_Quot1::(Integral a) => NonZero a -> NonZero a-> Bool
prop_Quot1 (NonZero x) (NonZero y) = quot x y * y + rem x y == x

prop_Quot2::(Integral a) => NonZero a -> NonZero a -> Bool
prop_Quot2 (NonZero x) (NonZero y) = div x y * y + mod x y == x

prop_PotCom::(Integral a) => a -> a -> Bool
prop_PotCom x y = x ^ y == y ^ x

prop_PotAssoc::(Integral a) => Positive a -> Positive a -> Positive a -> Bool
prop_PotAssoc (Positive x) (Positive y) (Positive z) = x ^ (y ^ z) == (x ^ y) ^ z

prop_RevRev::(Eq a ) => [a] -> Bool
prop_RevRev x = (reverse . reverse) x == id x

prop_F::(Eq b) => Fun a b -> a -> Bool
prop_F f x = applyFun f x == (applyFun f $ x)

prop_FComp::(Eq c) => Fun a b -> Fun b c -> a -> Bool
prop_FComp g f x = (applyFun f . applyFun g) x == (\y -> applyFun f (applyFun g y)) x

prop_foldr1:: (Eq a) => [a] -> [a] -> Bool
prop_foldr1 xs ys = ((foldr (:) ys xs)) == ((++) xs ys)
-- either you pass in different order or it doesn't pass the test
--prop_foldr1 xs ys = ((foldr (:) xs ys)) == ((++) xs ys)

prop_foldr2:: (Eq a) => [[a]] -> Bool
prop_foldr2 xs = ((foldr (++) [] xs)) == concat xs

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
  quickCheck (prop_Quot1::NonZero Int -> NonZero Int -> Bool)
  quickCheck (prop_Quot2::NonZero Int -> NonZero Int -> Bool)
  -- these does not hold even if only checking Nonzero or positive
--  quickCheck (prop_PotCom:: Int ->  Int ->  Bool)
--  quickCheck (prop_PotAssoc:: Positive Int -> Positive Int -> Positive Int -> Bool)
  quickCheck (prop_RevRev::String -> Bool)
  quickCheck (prop_F::Fun Char Int -> Char -> Bool)
  quickCheck (prop_FComp::Fun Char Int -> Fun Int String -> Char -> Bool)
  quickCheck (prop_foldr1::String -> String -> Bool)
  quickCheck (prop_foldr2::[String] -> Bool)

