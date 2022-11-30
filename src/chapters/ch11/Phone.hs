module Phone where

import Data.Char
import Data.List

data DaPhone = DaPhone [(Digit, String)]

sample =
  DaPhone
    [ ('1', "1"),
      ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "prqs7"),
      ('8', "tuv8"),
      ('9', "wxyz"),
      ('*', "^*"),
      ('0', " +_0"),
      ('#', ".,#")
    ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

-- Use elemIndex to find position
-- use foldr
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps p@(DaPhone (x : xs)) c
  | isUpper c = (reverseTaps p '^') ++ (reverseTaps p (toLower c))
  | otherwise = case elemIndex c (snd x) of
    Just n -> [((fst x), n + 1)]
    Nothing -> reverseTaps (DaPhone xs) c

fingerTaps::[(Digit, Presses)] -> Presses
fingerTaps = foldr ((+).snd) 0

messageToTaps::String -> [(Digit, Presses)]
messageToTaps = concat.map (reverseTaps sample) 

countTaps::String -> Presses
countTaps = fingerTaps.messageToTaps


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
  
instance Foldable BinaryTree where
  foldr _ z Leaf = z
  foldr  f acc (Node left a right) = foldr f (f a (foldr f acc right)) left
  
insert' :: (Ord a, Num b) => (a,b) -> BinaryTree (a, b) -> BinaryTree (a, b)
insert' x Leaf = Node Leaf x Leaf
insert' x@(c,d) (Node left y@(a,b) right)
  | c == a = Node left (a, b+d)  right
  | c < a = Node (insert' x left) y right
  | c > a = Node left y (insert' x right)


toTree::(Ord a) => [a] -> BinaryTree (a, Int)
toTree s = foldr insert' Leaf $ zip s (repeat 1)

 
mostPopular:: BinaryTree (a, Int) -> Maybe a
mostPopular Leaf = Nothing
mostPopular tree = result $ foldr maxBySnd Nothing tree
  where maxBySnd (a,x) Nothing = Just (a,x)
        maxBySnd (a,x) (Just (b,y)) = if (x>y) then Just(a,x) else Just(b,y)
        result Nothing = Nothing
        result (Just (a,_)) = Just(a)
  
mostPopularItem::(Ord a) => [a] -> Maybe a
mostPopularItem = mostPopular.toTree

-- More explicitly would be
--mostPopularLetter::String -> Maybe Char
--mostPopularLetter = mostPopular.toTree
--
--mostPopularWord::[String] -> Maybe String
--mostPopularWord = mostPopular.toTree 

-- Interesting
-- https://stackoverflow.com/questions/13426417/how-do-i-re-write-a-haskell-function-of-two-argument-to-point-free-style
cost::DaPhone -> Char -> Int
cost phone c = sum.(map snd) $ reverseTaps phone c 

coolestWord::[String] -> Maybe String
coolestWord  = mostPopularItem.concat.(map words) 

coolestLtr::[String] -> Maybe Char
coolestLtr  = mostPopularItem.concat

