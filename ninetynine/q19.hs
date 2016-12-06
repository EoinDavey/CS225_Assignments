-- rotate list N places to left
-- split list and swap lists

rotate :: [a] -> Int -> [a]
rotate ls n = last ++ first
        where (first, last) = splitAt ((n+(length ls)) `mod` length ls) ls

main = print $ rotate "abcdefgh" (-2)
