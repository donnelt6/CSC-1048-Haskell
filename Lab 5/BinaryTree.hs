-- Q1 Binary Search Tree

-- recursive data type representing a binary tree.
data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

--  This function creates a new tree with a single node containing the value x, where both its left and right children are empty.
leaf x = Root x Empty Empty

-- (a)
-- Write a recursive function addnode :: Ord a => a -> BinTree a -> BinTree a which, when 
--given an integer and a binary search tree, will add the integer at the correct position in the tree
-- takes two args, a of type Ord (so it can be compared)and a binary tree a | returns new binary tree a
addnode :: Ord a => a -> BinTree a -> BinTree a
-- base case
addnode x Empty = leaf x -- if empty tree, create tree with root x
-- recursive case
addnode x (Root n l r) -- root value represented by n
    | x < n            = (Root n (addnode x l) r) -- add to left subtree
    | otherwise        = (Root n l (addnode x r)) -- add to right subtree


-- (b)
-- will create a binary search tree by inserting the head of the list into the correct position in the tree created from the tail of the list.
maketree :: Ord a => [a] -> BinTree a
-- base case, only one element that becomes the root. the function adds the last element then recursviely backtracks to add the rest in their correct pos.
maketree (x:[]) = leaf x
-- recursive case, make use of addnode function
-- $ has a low precedence, meaning it allows you to apply functions without parentheses around their arguments.
maketree (x:xs) = addnode x $ maketree xs

-- (c)
--will return the list giving the result of an inorder traversal of the tree
inorder :: BinTree a -> [a]
-- base case, empty list, then funciton recursivley adds the elements to the list
inorder Empty = []
-- recursive case , n is the value of the current node, l is the left subtree, and r is the right subtree.
inorder (Root n l r) = inorder l ++ (n : inorder r)

-- (d)
--Monkey puzzle sort works by creating a binary search tree from a list, and then traversing the list in inorder
mpsort :: Ord a => [a] -> [a] 
mpsort xs = inorder $ maketree xs

-- Q7
--Higher Order Sort

hoaddnode :: Ord a => (a -> a -> Bool) -> a -> BinTree a -> BinTree a
hoaddnode _ x Empty         = leaf x
hoaddnode fn x (Root n l r)
    | fn x n                = (Root n (hoaddnode fn x l) r)
    | otherwise             = (Root n l (hoaddnode fn x r))

homaketree :: Ord a => (a -> a -> Bool) -> [a] -> BinTree a
homaketree _ (x:[])  = leaf x
homaketree fn (x:xs) = hoaddnode fn x $ homaketree fn xs

hosort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
hosort fn xs = inorder $ homaketree fn xs