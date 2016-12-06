-- list given range

range :: (Ord a, Enum a) => a -> a -> [a]
-- range a b = [a..b] trivial answer. cheating
range a b
    | a > b = []
    | otherwise = a : range (succ a) b

main = print $ range 4 9
