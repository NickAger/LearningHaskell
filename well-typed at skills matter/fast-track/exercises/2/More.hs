module More where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

--------------------------------------------------------------------------
-- Introduction:
--
-- Some more exercises using higher-order functions, type classes and
-- finite maps.
--
-- Note that not all module imports are listed above. At certain points,
-- you may/will have to add import statements yourself.

--------------------------------------------------------------------------
-- Function composition:

-- Let's go back to the beginning. Among the List exercises, there was
-- one that told you to compute the sum of squares of even numbers:
--
-- The function 'sumEvenSquares' should go through a list of integers,
-- keep only the even ones, square each of them, and compute the sum
-- of these.
--
-- Example:
--
--   sumEvenSquares [1,2,3,4] = 20
--
-- because (2 * 2) + (4 * 4) = 20.
--
-- If you followed the standard design principle for lists, you will
-- have ended up with something like the following:

sumEvenSquares :: [Int] -> Int
sumEvenSquares []       = 0
sumEvenSquares (x : xs)
  | even x              = x * x + sumEvenSquares xs
  | otherwise           =         sumEvenSquares xs

-- Now try to rewrite the function as a composition of standard
-- functions. Try to actually use the (.) operator.

-- Similarly, in BasicFunctions, you were supposed to define a function
-- 'encode'. Try to rewrite its definition as a composition using (.)
-- ...

type Runlength a = [(Int, a)]

encode :: Eq a => [a] -> Runlength a
encode = error "TODO: (re)define encode"

--------------------------------------------------------------------------
-- Node-labeled binary trees:

data BinTree a = Bin (BinTree a) a (BinTree a) | Empty
  deriving (Show)

-- The 'BinTree' type above is different from 'Tree' we have seen before
-- in that it has labels in the nodes rather than leaves.

-- Define a function 'size' for 'BinTree', counting the number of
-- 'Bin'-nodes.

size :: BinTree a -> Int
size = error "TODO: define size"

-- Define your own 'Eq' instance for 'BinTree'. Yes, we could
-- derive them, but this is to practice.

-- Next, define your own 'Functor' instance for 'BinTree'.

--------------------------------------------------------------------------
-- Binary search trees:

-- The 'BinTree' type is suitable for representing "binary search
-- trees". Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.

-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.

isBST :: Ord a => BinTree a -> Bool
isBST = error "TODO: define isBST"

-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree:

type BST a = BinTree a

-- Define a function 'search' that looks up a value in a BST.

search :: a -> BST a -> Bool
search = error "TODO: define search"

-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property.

insert :: a -> BST a -> BST a
insert = error "TODO: define insert"

--------------------------------------------------------------------------
-- Higher-order functions:

-- Define function 'all' using 'foldr'. Proceed in a number of steps:
--
--   First, query GHCi about the type of 'all'.
--   Then figure out what the function does by looking at examples.
--   Finally, hide the import of 'all' in the module header, and
--   give a new definition using 'foldr'.

-- In BasicFunctions, you have defined the function 'group'. Now, define
-- 'group' again using 'foldr'.

-- A slightly more challenging task it to try to write the function 'take'
-- using 'foldr'. It's possible, but requires a very functional mindset.
-- Try it yourself, perhaps also as a challenge for tonight, but also don't
-- be afraid to ask for help if you think you need any.

-- Import the function 'sortBy' from the 'Data.List' module (only this
-- function). Then use this function, and preferable other already existing
-- functions, to define a function

sortDescending :: Ord a => [a] -> [a]
sortDescending = error "TODO: define sortDescending"

-- that sorts a list in descending rather than ascending order.

--------------------------------------------------------------------------
-- Depth-first search

-- Let us assume we have a representation of directed graphs that is
-- as follows:

type Node  = Int
type Graph = Map Node [Node]

-- For simplicity, we assume nodes are identified by numbers. For every
-- node, we store the successors of the node in a finite map.
--
-- Here's an example of a cyclic graph with three nodes:

cyclic :: Graph
cyclic = M.fromList [(1, [2]), (2, [3]), (3, [1])]

-- Here's an example of a binary tree:

tree :: Graph
tree = M.fromList
  [ (1, [2,3])
  , (2, [4,5])
  , (3, [6,7])
  , (4, []   )
  , (5, []   )
  , (6, []   )
  , (7, []   )
  ]

-- (1 is the root; 2 and 3 are on the next level; 4,5,6,7 are
-- leaves).

-- A graph is only valid if all nodes being mentioned in successor lists
-- are also among the keys. Write a function that tests this.

validGraph :: Graph -> Bool
validGraph = error "TODO: define validGraph"

-- Optional: write a function 'complete' that takes a possibly invalid graph
-- and adds empty entries to the finite map for nodes that are mentioned in
-- successor lists, but do not have their own entries.
--
-- For example,
--
--   complete (M.fromList [(1, [2,3]), (2, [4,5]), (3, [6,7])])
--
-- should yield 'tree'.
--
-- Hint: Perhaps you can use the 'M.insertWith' or the 'M.alter'
-- functions on finite maps.

complete :: Graph -> Graph
complete = error "TODO: optionally define complete"

-- Write a function that tests if a given node exists in a given graph
-- and has no successors.

existsWithNoSuccessors :: Node -> Graph -> Bool
existsWithNoSuccessors = error "TODO: define existsWithNoSuccessors"

-- Try to implement "depth first search":
--
-- from a given starting node 'n', 'dfs n' should return all the nodes
-- that are reachable from 'n' in the order they're visited by depth-first
-- search.

dfs :: Graph -> Node -> [Node]
dfs = error "TODO: define dfs"
  where
    go :: [Node] -> [Node] -> [Node]  -- suggestion, see below
    go = error "TODO: complete go"

-- This function should return the empty list if the given
-- node is not in the graph. It may assume that the graph
-- is valid, and may fail on invalid graphs.
--
-- Here's a typical imperative description of the DFS algorithm,
-- like you could find it on the internet.
--
-- Inputs: A Graph G and a node n.
-- Outputs: A list of nodes.
--
-- Maintain a list of visited nodes vs, and a stack of nodes
-- to be visisted cs.
--
-- initialize vs to be empty.
-- DFS(G, n):
--    initialize vs to be empty
--    initialize cs to contain just n
--    while cs is not empty:
--       pop the topmost element c from cs
--       if c is not in vs:
--         output c
--         add c to vs
--         for all successors s of n in G:
--            push s on top of cs
--
-- Try to transcribe this into a functional algorithm by using
-- a helper function that takes both the visited nodes and the
-- stack as arguments, and "updates" these arguments in the
-- recursive calls -- quite similar to the accumulated parameter
-- technique.

-- Here's a slightly more interesting example graph for you
-- to test with:

graph :: Graph
graph = M.fromList
  [ (1, [])
  , (2, [1,3,4])
  , (3, [2,4])
  , (4, [2,5,7])
  , (5, [6])
  , (6, [])
  , (7, [3])
  ]

-- If your algorithm works and if you like, you can try to make it
-- more efficient on really large graphs by turning the data
-- structure for the visited nodes into a Set.
