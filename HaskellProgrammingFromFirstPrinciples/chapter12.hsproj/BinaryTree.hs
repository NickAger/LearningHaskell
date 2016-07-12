module BinaryTree where
  
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Ord, Eq)
  
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f s = go (f s)
  where
      go Nothing = Leaf
      go (Just (v1, b, v2)) = Node (unfold f v1) b (unfold f v2)
      
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeIterator 0
  where
    treeIterator :: Integer -> Maybe (Integer,Integer,Integer)
    treeIterator n'
      | n' == n = Nothing
      | otherwise = Just (n'+1, n', n'+1)
