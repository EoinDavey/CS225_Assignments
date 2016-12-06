-- run length encoding

get :: (Eq a) => [a] -> a -> ([a],[a])
get [] _      = ([],[])
get (x:xs) c  = let (first, last) = get xs c in if (x==c) then (x:first, last) else (first, x:last)

pack :: (Eq a) => [a] -> [[a]] 
pack []       = []
pack l@(x:xs) = let (f,la) = get l x in f:pack la

encode :: (Eq a) => [a] -> [(Int,a)]
encode l = map (\xs -> (length xs, head xs)) $ pack l

main = print $ encode "aaaaaabccddddde"
