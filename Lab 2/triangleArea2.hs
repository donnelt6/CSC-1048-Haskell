
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
    | not (isTriangle a b c) = error "Not a triangle!"
    | otherwise =
        let s = (a+b+c) / 2 
        in sqrt (s * (s-a) * (s-b) * (s-c)) 

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c =
    (a + b > c) && (b + c > a) && (a + c > b)

main :: IO ()
main = print (triangleArea 3 4 5)