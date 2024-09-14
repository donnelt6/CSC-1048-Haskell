evalPoly :: Int -> [Int] -> Int
-- Base case , the list containing the polynomial is empty, serves as stopping point for recursion
evalPoly _ []     = 0
-- Recursive case
-- x is the valye at which we want to eval the poly. y is the first term in the poly, ys is the rest of the terms.
-- evalPoly x ys represents the value of the polynomial for the remaining terms. It is computed recursively.
-- (x * (evalPoly x ys) . This term scales the result of the recursive evaluation by x, effectively "shifting" it to the next term in the polynomial. 
-- Finally, we add y to this scaled result - our constant
evalPoly x (y:ys) = y + (x * (evalPoly x ys)) 

main :: IO ()
main = print (evalPoly 3 [1,7,5,2])
-- evaluates to 121
-- 1 + 7x + 5x^2 + 2x^3
