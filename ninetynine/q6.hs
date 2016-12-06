-- palindrome
pal :: (Eq a) => [a] -> Bool
pal []  = True
pal [_] = True
pal xs  = (head xs) == (last xs) && (pal $ tail $ init xs)

main = print $ pal [1,2,3,2,1]
