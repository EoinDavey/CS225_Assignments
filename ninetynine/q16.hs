-- drop every N'th element

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery ls n = drop ls n
    where drop [] _     = []
          drop (x:xs) 1 = drop xs n
          drop (x:xs) r = x : drop xs (r-1)

main = print $ dropEvery "abcdefghik" 3
