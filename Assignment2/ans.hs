module Assignment2 where
------------------------------------
--Recursive Patterns
--q1:
data Bit = Zero | One
    deriving (Eq,Show)

xor :: Bit -> Bit -> Bit
i `xor` j
    | i==j      = Zero
    | otherwise = One

oddOnes :: [Bit] -> Bit
oddOnes bs = foldl xor Zero bs

--q2:
filter' :: (a -> Bool) -> [a] -> [a]
filter' f as = foldr (\x -> if f x 
                            then (x:)
                            else (id)) [] as

map' :: (a -> b) -> [a] -> [b]
map' f as = foldl (\z x -> z++[(f x)]) [] as

--------------------------------------------------
-- TypeClasses
-- q1: N={1,2,..} cannot be a monoid set under addition because the addition identity element (zero) is not in the set N.
-- q2: End(A) can form a monoid under the composition operation, the identity of the composition function is just e(x)->x, e.f(x) = f.e(x) = f(x)
-- q3:

data Failable a = Failure String | OK a
    deriving Show

instance Functor Failable where
    fmap f (Failure s) = Failure s
    fmap f (OK a)      = OK (f a)
