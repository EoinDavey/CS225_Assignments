-- split list at n'th pos

split :: [a] -> Int -> ([a],[a])
split ls 0     = ([],ls)
split [] _     = ([],[])
split (x:xs) n = let (b,e) = split xs (n-1) in (x:b,e)

main = print $ split "abcdefghijkl" 3
