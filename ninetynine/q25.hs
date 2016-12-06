-- gen rnd perm
import System.Random

perm :: [a] -> IO [a]
perm [] = return []
perm ls = do pos <- randomRIO (0, (length ls) -1)
             let rem = take pos ls ++ drop (pos+1) ls
             rest <- perm rem
             return ((ls!!pos): rest)
main = print =<< perm [1..10]
