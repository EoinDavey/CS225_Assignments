data IntTree = End | Node Int IntTree IntTree
    deriving (Show)

inOrder::IntTree -> [Int]
inOrder End = []
inOrder (Node x left right) = (inOrder left) ++ [x] ++ (inOrder right)

topDown::IntTree -> [Int]
topDown t = [x | y<-[0..mx],(x,d) <- ps, y==d]
      where ps = depthTraverse t 0
            mx = maximum $ map (snd) $ ps

bottomUp::IntTree -> [Int]
bottomUp t = [x | y<-[mx,(mx-1)..0],(x,d) <- ps, y==d]
      where ps = depthTraverse t 0
            mx = maximum $ map (snd) $ ps

depthTraverse::IntTree -> Int -> [(Int,Int)]
depthTraverse End _ = []
depthTraverse (Node x left right) d = (depthTraverse left (d+1)) ++ [(x,d)] ++ (depthTraverse right (d+1))

main = print $ inOrder (Node 3 (Node 1 End (Node 2 End End)) (Node 5 (Node 4 End End) (Node 6 End End)))
