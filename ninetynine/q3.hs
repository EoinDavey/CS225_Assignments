-- element at
elementat::[a] -> Int -> a
elementat (x:xs) 1 = x
elementat (x:xs) n = elementat xs (n-1)

main = print $ elementat ['a'..'z'] 5
