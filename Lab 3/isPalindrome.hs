isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == reverse a

main :: IO ()
main = print (isPalindrome "madame")


-- The (Eq a) constraint is necessary because we are using the (==) operator
-- to compare the original list 'a' with its reversed version 'reverse a'.
-- The (==) operator requires that the elements of the list must be comparable
-- for equality, which is provided by the Eq typeclass. Without this constraint,
-- we wouldn't be able to check if the two lists are equal.









