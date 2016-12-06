-- remove K'th element

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 1 (x:xs) = (Just x,xs)
removeAt n (x:xs) = let (a, ys) = removeAt (n-1) xs in (a, x:ys)

main = print $ removeAt 2 "abcd"
