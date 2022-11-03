module Trees where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"

------------------------------------------
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node Leaf x Leaf) = [x]
preorder (Node lt x rt) =  [x] ++ preorder lt ++ preorder rt

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node Leaf x Leaf) = [x]
inorder (Node lt x rt) = inorder lt ++ [x] ++ inorder rt

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node Leaf x Leaf) = [x]
postorder (Node lt x rt) =   postorder lt ++ postorder rt ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears"

-- probably there's a more elegant way of doing this
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f zb tree = foldr f zb (inorder tree)

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder