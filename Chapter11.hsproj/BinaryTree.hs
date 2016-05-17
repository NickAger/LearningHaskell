module BinaryTree where
  
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)
  
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)
  
testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
  
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
  
-- acceptance test for mapTree
mapOkay =  if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"    else error "test failed!"


preorder :: BinaryTree a -> [a]
preorder a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = (a:list) ++ (go list left) ++ (go list right)
    
inorder :: BinaryTree a -> [a]
inorder a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = (go list left) ++ (a:list) ++ (go list right)
    
postorder :: BinaryTree a -> [a]
postorder a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = (go list left) ++ (go list right) ++ (a:list)
  
-- now I've implemented foldTree I'm rewriting these without the need for `++`
preorder2 :: BinaryTree a -> [a]
preorder2 a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = a:go (go list right) left

inorder2 :: BinaryTree a -> [a]
inorder2 a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = go (a:go list right) left
    
postorder2 :: BinaryTree a -> [a]
postorder2 a = go [] a
  where
    go :: [a] -> BinaryTree a -> [a]
    go list Leaf = list
    go list (Node left a right) = go (go (a:list) right) left
 
    
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
    else putStrLn "Bad news bears."
    
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f s Leaf = s
foldTree f s (Node left a right) = foldTree f (f a (foldTree f s right)) left

testTreeFoldOrder :: Bool
testTreeFoldOrder = foldTree (:) [] testTree == [1,2,3]
