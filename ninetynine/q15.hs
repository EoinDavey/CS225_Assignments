-- replicate

rep :: a -> Int -> [a]
rep _ 0 = []
rep a n = a : rep a (n-1)

repL :: [a] -> Int -> [a]
repL ls n = foldr (\a acc -> (rep a n)++ acc) [] ls

main = print $ repL "abbc" 4
