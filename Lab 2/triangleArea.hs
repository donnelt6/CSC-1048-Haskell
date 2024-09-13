
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = 
    let s = (a+b+c) / 2 
    in sqrt (s * (s-a) * (s-b) * (s-c)) 

main :: IO ()
main = print (triangleArea 1 1 (sqrt 2))
    
