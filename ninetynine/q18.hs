-- splice
splice :: [a] -> Int -> Int -> [a]
splice [] _ _ = []
splice ls b e = break ls 1
    where break (x:xs) c
            | c > e = []
            | c >= b = x : break xs (c+1)
            | otherwise = break xs (c+1)

main = print $ splice "abcdefghik" 3 7
