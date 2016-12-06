-- direct run-length encoding

data Data a = Single a | Multiple (Int, a)
    deriving Show

encode :: (Eq a) => [a] -> [(Int,a)]
encode ls = foldr helper [] ls
    where helper x [] = [(1,x)]
          helper x ((a,b):ys) = if b == x then (1+a,b):ys else (1,x):(a,b):ys

single :: (Eq a) => [a] -> [Data a]
single ls = map (change)$encode ls
    where change (1,x) = Single x
          change (n,x) = Multiple (n,x)

main = print $ single "aabccdeee"
