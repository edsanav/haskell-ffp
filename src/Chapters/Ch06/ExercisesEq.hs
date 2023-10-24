module ExercisesEq where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn n2) = n == n2
  
data TwoIntegers =
 Two Integer Integer
 
instance Eq TwoIntegers where
  (==) (Two n m) (Two n2 m2) = n == n2 && m == m2 

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair v' w2) = v == v' && w == w2
  
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
   (==) (Tuple x y) (Tuple x' y') = x==x' && y == y'
   
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False
  
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False
  