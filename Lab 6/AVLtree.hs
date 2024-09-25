-- Implement AVL trees in Haskell and in particular two functions:
-- insert a values into an AVL tree (maintaining its AVL properties); and  display an AVL tree. 



data AVLTree t = Empty
               | Root t (AVLTree t) (AVLTree t)
                 deriving (Eq, Show)

leaf x = Root x Empty Empty

-- Balance Left-Left (LL) Case
-- This means that the current node has a left child (Root n1 l1 r1), which itself has a left child. This is the LL case of imbalance.
balanceLL :: (Ord a) => AVLTree a -> AVLTree a
balanceLL (Root n (Root n1 l1 r1) r) = Root n1 l1 (Root n r1 r)

-- This function handles the right-right case.
balanceRR :: (Ord a) => AVLTree a -> AVLTree a
balanceRR (Root n l (Root n1 l1 r1)) = Root n1 (Root n l l1) r1

showTree :: (Ord a) => AVLTree a -> String
showTree Empty = ""
showTree (Root x l r) = ("[ " ++ x ++ "] " ++ (showTree l) ++ (showTree r))
