-- insert at given pos

insertAt :: a -> [a] -> Int -> [a]
insertAt i ls 1     = i:ls
insertAt _ [] _     = []
insertAt i (x:xs) n = x : insertAt i xs (n-1)

main = print $ insertAt 'X' "abcd" 2
