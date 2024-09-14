-- type synonym for Poly
type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly

-- Base cases (two empty lists, one empty list)
sumPoly [] []         = []
sumPoly a []          = a
sumPoly [] a          = a
-- Recursive case 
-- x and y are the first element in their respective polynomials. add them. then recall the recursive function to act on (add) the remaining terms of the polynomials, until one of the bases cases are reached
-- this results in our new polynomial, a result of adding the other polynomials
sumPoly (x:xs) (y:ys) = (x + y) : sumPoly xs ys

main :: IO ()
main = print (sumPoly [1,2,3,4] [2,3,4,5])