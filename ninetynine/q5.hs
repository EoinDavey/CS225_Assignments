-- reverse list
rev::[a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

main = print$ rev [1..5]
