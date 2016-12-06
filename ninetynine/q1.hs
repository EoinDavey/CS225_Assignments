last' :: [a] -> a
last' [x] = x
last' (x:xs) = last xs

main = print $ last' [1..5]
