-- show State instances for Fluffy and Misty
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

class Fluffy f where
  furry :: (a -> b) -> f a -> f b


newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f sa = State (getState)
      where getState s = (g, f a)
                where (g, a) = state sa s

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f sa = State (getState2) 
       where getState2 s = state (f a) g
                where (g, a) = state sa s

  unicorn a = State (\s -> (s, a))
-- /show

main = putStrLn "It typechecks!"
