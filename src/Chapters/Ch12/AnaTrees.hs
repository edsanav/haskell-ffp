module AnaTrees where

  
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f seed = case f seed of
  Nothing -> Leaf
  Just (seedLeft, x, seedRight) -> Node (unfold f seedLeft) x (unfold f seedRight) 

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where 
  f x  
    | x == n = Nothing
    | otherwise = Just (x+1, x, x+1) 