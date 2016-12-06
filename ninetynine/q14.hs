-- duplicate

dup :: [a] -> [a]
dup []     = []
dup (x:xs) = [x,x] ++ dup xs

main = print $ dup [1,2,3]
