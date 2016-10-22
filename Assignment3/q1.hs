main::IO()
main = do
    n <- readLn::IO Int
    print $ leapYear $ n

leapYear :: Int -> Bool
leapYear n = if n `mod` 400 == 0 then True
    else if n`mod`100 == 0 then False
    else if n`mod`4== 0 then True
    else False
