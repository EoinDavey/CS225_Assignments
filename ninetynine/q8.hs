-- compress

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x:group' xs x

group' :: (Eq a) => [a] -> a -> [a]
group' [] _     = []
group' (x:xs) c = if x /= c then [x] ++ (group' xs x) else group' xs c

main = print $ compress "aaaaabbbbcccdcccd"
