isSum :: Int -> Int -> Int -> Bool
isSum a b c =
    (a + b) == c

main :: IO ()
main = print(isSum 1 2 2)