class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana _ []     = []
  banana f (x:xs) = (f x) ++ banana f xs
  unicorn x       = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing  = Nothing
  banana f (Just a) = f a
  unicorn a         = Just a

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana a b c = (a (b c)) c
  unicorn a _  = a

newtype EitherLeft b a  = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x))  = f x
  banana _ (EitherLeft (Right x)) = EitherLeft (Right x)
  unicorn a                       = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  banana _ (EitherRight (Left x))  = EitherRight (Left x)
  unicorn a                        = EitherRight (Right a)

main = putStrLn "It typechecks!"
