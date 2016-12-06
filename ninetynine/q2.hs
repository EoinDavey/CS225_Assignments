-- second from last

last' :: [a] -> a
last' l 
    | (length l == 2) = head l
    | otherwise = last'$tail l

main = print $ last' ['a'..'z']
