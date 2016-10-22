main::IO()
main = do
    input <- getLine
    putStrLn $ init $parse input

parse::String -> String
parse [] = []
parse (x:xs) = toWord x ++ parse xs

toWord::Char -> String
toWord '1' = "one-"
toWord '2' = "two-"
toWord '3' = "three-"
toWord '4' = "four-"
toWord '5' = "five-"
toWord '6' = "six-"
toWord '7' = "seven-"
toWord '8' = "eight-"
toWord '9' = "nine-"
toWord '0' = "zero-"
