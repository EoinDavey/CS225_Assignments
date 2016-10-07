main::IO()
main = print("Test")

data IntTree = End | Sub Int IntTree IntTree
    deriving (Show)

inOrder::IntTree -> [Int]
inOrder End = []
inOrder (Sub x left right) = (inOrder left) ++ [x] ++ (inOrder right)
