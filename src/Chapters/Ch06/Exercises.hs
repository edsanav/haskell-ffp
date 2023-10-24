module Exercises where

import Data.List (sort)
  
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah | Woot deriving Show


instance Eq Mood where
  Blah == Blah = True
  Woot == Woot = True
  _ == _ = False

settleDown x = if x == Woot then Blah else x
  
 
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu  (Rocks "chases") (Yeah True)

phew2 = Papu  (Rocks "chases") (Yeah False)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- not a proper instance, but as an example
instance Ord Papu where
  compare (Papu x y) (Papu x' y') = compare (compare y y') (compare x x')


comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


i :: Num a => a
i = 1

f' :: Float
f' = 1.0

f2 :: Fractional a => a
f2 = 1.0

f3 :: RealFrac a => a
f3 = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

sigmund' :: Int -> Int
sigmund' x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b 
arith f int x = f x + fromInteger int

