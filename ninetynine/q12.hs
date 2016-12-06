data Data a = Single a | Multiple (Int, a)
    deriving Show

decode :: [Data a] -> [a]
decode [] = []
decode (x:xs) = (case x of
                    (Single a) -> [a]
                    (Multiple (n,a)) -> replicate n a) ++ decode xs

main = print $ decode [Multiple (4,'a'), Single 'b', Multiple (2,'c')]
