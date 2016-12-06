-- select given number of randomly selected elements
import System.Random
import Control.Monad

rnd :: [a] -> Int -> IO [a]
rnd [] _ = return []
rnd ls n = replicateM n $ do pos <- getStdRandom $ randomR (0, (length ls) -1)
                             return (ls!!pos)
main = print =<< rnd ['a'..'h'] 3
