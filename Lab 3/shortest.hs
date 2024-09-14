shortest :: [[a]] -> [a]
-- Two base cases, empty list and only one list
shortest []               = []
shortest [x]              = x
-- Recursive case (at least two lists)
-- Uses pattern matching. x is the first list
-- ys@(y:xs) indicates y is the second list, where xs is the remaining lists after ( rest of the list of lists) 
-- ys represents y and the rest of the lists (y : xs)
-- ":" seperates head from tail in haskell
shortest (x:ys@(y:xs))
-- discard the second list if the first list is shorter. we now recall recursively on x and the rest of the lists(xs)
    | length x < length y = shortest (x : xs)
-- discard the first list if the second list is shorter. we now recall recursively on y and the rest of the lists (ys)
    | otherwise           = shortest ys


main :: IO ()
main = print (shortest [[1,2],[1,2,3,4,5],[1],[1,2,3],[]])