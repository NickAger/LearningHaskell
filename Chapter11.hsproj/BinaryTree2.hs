module BinaryTree2 where


-- an experiment to make a Binary tree where the values are stored at the leafs not the nodes.
-- trouble is it makes insertion problematic as I can't tell where to insert values in the tree.
-- so experiment aborted
  
data BinaryTree2 a =
    Leaf a
  | Node (BinaryTree2 a) (BinaryTree2 a)
  deriving (Eq, Ord, Show)
  


--insert' :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
--insert' b (Leaf a) 
--  | b == a = Leaf a
--  | b < a = Node (insert' b left) a right
--  | b > a = Node left a (insert' b right)
--
--Node Leaf b Leaf
--insert' b (Node left a right)
--  | b == a = Node left a right
--  | b < a = Node (insert' b left) a right
--  | b > a = Node left a (insert' b right)