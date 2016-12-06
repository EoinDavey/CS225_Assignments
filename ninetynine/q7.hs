data NestedList a = Elem a | List [NestedList a]

flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List ls) = foldr (++) []$map (flatten') ls

main = print $ flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
