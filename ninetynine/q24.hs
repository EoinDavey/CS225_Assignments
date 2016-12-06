-- Lotto: Draw n different random numbers from the set 1..M

import System.Random

diff_select :: Int -> [Int] -> IO [Int]
diff_select _ [] = return []
diff_select 0  _ = return []
diff_select n ls = do pos <- randomRIO (0, (length ls)-1)
                      let remaining = take pos ls ++ drop (pos+1) ls
                      rest <- diff_select (n-1) remaining
                      return ((ls!!pos) : rest)

main = print =<< diff_select 6 [1..47]
