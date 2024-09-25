{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import System.Win32 (xBUTTON1)
-- Develop my own list functions using recursion

myAppend :: [a]->[a]->[a]
-- base cases
myAppend [] [] = []
myAppend a []  = a
myAppend [] a  = a
-- recursive case (:) prepends an element to a list
myAppend (x:xs) ys = x : myAppend xs ys

myHead :: [a]->a
-- base case
myHead [] = error "List has no head as it is empty"
-- recursive case
myHead (x:xs) = x

myLast :: [a]->a
-- base case
myLast [] = error "Empty List has no last element"
myLast [x] = x -- our last element
-- recursive case , recursively calls until we are left with one element - our last
myLast (_:xs) = myLast xs

myTail :: [a]->[a]
-- base case
myTail [] = error "List must be non empty"
-- recursive case
myTail (_:xs) = xs

myInit :: [a]->[a]
-- base case
myInit [] = error "List must be non empty"
myInit (x:[]) = [] -- when there is one element left, return empty list, each element is then prepended to this last then as recursion unwinds, except the last element
-- recursive case
myInit (x:xs) = x : myInit xs

myLength :: [a]->Int
-- base case = 0
myLength [] = 0
-- recursive case
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
-- base case, empty list
myReverse [] = []
-- recursive case prepend the last element to the list, keeps recursively calling until and empty list
myReverse xs = last xs : myReverse (init xs)

myConcat :: [[a]] -> [a]
-- base case
myConcat [] = []
-- recursive case
myConcat (x:xs) = x ++ myConcat xs

mySum :: Num a => [a] -> a
-- base case
mySum [] = 0
-- recursive case
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
-- base case
myProduct [] = 1 -- as we will multiply by this , and it needs to be 1 instead of 0
-- recursive case
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a
-- base case
myMaximum [x] = x
-- recursive case as recursion unwinds, the comparisons are made
myMaximum (x:xs) = max x (myMaximum xs)

myMinimum :: Ord a => [a] -> a
-- base case
myMinimum [x] = x
-- recursive case
myMinimum (x:xs) = min x (myMinimum xs)


-- the list memebership predicate, checks if x is in list y, returns True or False
myElem :: Eq a => a -> [a] -> Bool
-- base case
myElem x []     = False
-- recursive case
myElem x (y:ys)
    | x == y    = True
    | otherwise = myElem x ys -- keep searching until you have an empty list

-- removes the first occurence of x from a list
myDelete :: Eq a => a -> [a] -> [a]
-- base case 
myDelete x []     = []
-- recursive case
myDelete x (y:ys)
    | x == y      = ys -- if x is the first element , return just the tail
    | otherwise   = y : myDelete x ys -- otherwise keep comparing until empty list


-- Set functions for lists

-- Returns the list union of two lists. Duplicates, and elements of the first list, are removed from the the
-- second list, but if the first list contains duplicates, so will the result

myUnion :: Eq a => [a] -> [a] -> [a]
-- base case
myUnion xs [] = xs
-- recursive case
myUnion xs (y:ys)
    | y `elem` xs = myUnion xs ys -- if y is in xs (duplicate) , remove it
    | otherwise   = myUnion (xs ++ [y]) ys -- else concatenate y to xs and continue the process


-- Returns the intersect of two lists

myIntersect :: Eq a => [a] -> [a] -> [a]
-- base cases
myIntersect x []      = []
myIntersect [] y      = []
-- recursive case
myIntersect (x:xs) ys
    | x `elem` ys     = x : myIntersect xs ys -- if intersect, then append to the empty list that will be left at the end of recursion
    | otherwise       = myIntersect xs ys -- otherwise keep searching





main :: IO ()
main = do
    print $ myAppend [1, 2, 3] [4, 5, 6]  -- Output: [1,2,3,4,5,6]
    print $ myHead [1, 2, 3, 4, 5] -- Output: 1
    print $ myLast [1, 2, 3, 4, 5] -- Output: 5
    print $ myTail [1, 2, 3, 4, 5] -- Output: [2,3,4,5]
    print $ myInit [1, 2, 3, 4, 5] -- Output: [1,2,3,4]
    print $ myLength [1, 2, 3, 4, 5] -- Output: 5
    print $ myReverse [1, 2, 3, 4, 5] -- Output: [5,4,3,2,1]
    print $ myConcat [[1, 2], [3, 4], [5, 6]] -- Output: [1, 2, 3, 4, 5, 6]
    print $ mySum [1, 2, 3, 4, 5] -- Output: 15
    print $ myProduct [1, 2, 3, 4, 5] -- Output: 120
    print $ myMaximum [3, 1, 4, 1, 5, 9] -- Output: 9
    print $ myMinimum [3, 1, 4, 1, 5, 9] -- Output: 1
    print $ myElem 3 [3, 1, 4, 1, 5, 9] -- Output: True
    print $ myElem 10 [3, 1, 4, 1, 5, 9] -- Output: False
    print $ myDelete 1 [3, 1, 4, 1, 5, 9] -- Output: [3,4,1,5,9]
    print $ myUnion [1,3,5,1] [2,2,3,4] -- Output: [1,3,5,1,2,4] 
    print $ myIntersect [1,2,2,3,4] [6,4,4,2] -- Output: [2,2,4] 