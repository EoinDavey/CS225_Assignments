import Data.Char
import Control.Monad
import Control.Applicative
main::IO()
main = do
    t <- readLn::IO Int
    replicateM_ t (do
            inp <- getLine
            let s = words $ inp
            let n = head s
            let [b,nb] = parseLine $ tail s
            putStrLn $ if n=="0" then "0" else convert n b nb
            )

parseLine :: [String] -> [Int]
parseLine = map read

convert::String->Int->Int->String
convert n b nw = fromBaseTen nw f
    where f = toBaseTen b n

toBaseTen::Int->String->Int
toBaseTen b n = foldl (\x y -> b*x + (digitToInt y)) 0 n

fromBaseTen::Int->Int->String
fromBaseTen _ 0  = ""
fromBaseTen b n  = (fromBaseTen b (n `div` b)) ++ (show $ n`mod`b)
