module MyEnum where

eftBool:: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool _ _ = []

eftOrd::Ordering -> Ordering -> [Ordering]
eftOrd  LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x <= y = x:(eftInt (x+1) y)
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x <= y = x:(eftChar (succ x) y )
  | otherwise = []

eftGeneral :: (Enum a, Ord a) => a -> a -> [a]
eftGeneral x y
  | x < y = x:(eftGeneral (succ x) y)
  | x == y = [y]
  | otherwise = []