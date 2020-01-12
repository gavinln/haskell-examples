{-
Demonstrates using datatype Mabye2 how to create typeclasses:
Functor2, Applicative2, Monad2

Use existing typeclasses: Functor, Applicative, Monad

and use: fmap, <*>, <$>, >>= and do notation

-}

inc :: Num a => a -> a
inc x = x + 1

add :: Num a => a -> a -> a
add x y = x + y

data Maybe2 a = Nothing2 | Just2 a deriving Show

class Functor2 f where
  fmap2 :: (a -> b) -> f a -> f b

instance Functor2 Maybe2 where
  -- fmap :: (a -> b) -> f a -> f b

  fmap2 _ Nothing2  = Nothing2
  fmap2 g (Just2 x) = Just2 (g x)

-- fmap2 inc (Just2 2)
-- Just2 3

class Functor2 f => Applicative2 f where
  pure2 :: a -> f a
  apply2 :: f (a -> b) -> f a -> f b

instance Applicative2 Maybe2 where
  -- pure :: a -> Maybe a
  pure2 x = Just2 x

  -- apply2 :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
  apply2 (Just2 g) mx = fmap2 g mx

-- apply2 (pure2 inc) (Just2 2)
-- Just2 3
-- apply2 (pure2 inc) Nothing2
-- Nothing2

class Applicative2 m => Monad2 m where
  return2 :: a -> m a
  return2 x = pure2 x

  -- (>>=) :: m a -> (a -> m b) -> m b
  bind2 :: m a -> (a -> m b) -> m b

instance Monad2 Maybe2 where
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- bind :: m a -> (a -> m b) -> m b
  bind2 Nothing2 _ = Nothing2
  bind2 (Just2 x) f = f x

inc_maybe2 :: Num a => a -> Maybe2 a
inc_maybe2 x = Just2 (x + 1)

-- bind2 (Just2 2) inc_maybe2
-- Just2 3

-- bind2 (Nothing2) inc_maybe2
-- Nothing2

-- bind2 (bind2 (Just2 2) inc_maybe2) inc_maybe2
-- Just2 4

add_maybe2 :: Num a => a -> a -> Maybe2 a
add_maybe2 x y = Just2 (x + y)

-- bind2 (Just2 2) (add_maybe2 1)
-- Just2 3

-- bind2 (Just2 2) (\x -> bind2 (Just2 1) (\y -> add_maybe2 x y))
-- Just2 3

-- bind2 (Just2 2) (\x -> bind2 (Nothing2) (\y -> add_maybe2 x y))
-- Nothing2

instance Functor Maybe2 where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Nothing2 = Nothing2
  fmap f (Just2 x) = Just2 (f x)

-- fmap inc (Just2 2)
-- Just2 3

instance Applicative Maybe2 where
  -- pure :: a -> Maybe a
  pure x = Just2 x

  -- (<*>) :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
  (Just2 g) <*> mx = fmap g mx

-- (pure inc) <*> (Just2 2)
-- Just2 3

-- inc <$> (Just2 2)
-- Just2 3

-- add <$> (Just2 2) <*> (Just2 1)
-- Just2 3

instance Monad Maybe2 where
  -- (>>=) :: m a -> (a -> m b) -> m b
  Nothing2 >>= _ = Nothing2
  (Just2 x) >>= f = f x

-- (Just2 2) >>= inc_maybe2
-- Just2 3

-- (Just2 2) >>= inc_maybe2 >>= inc_maybe2
-- Just2 4

-- (Just2 2) >>= \x -> return (inc x)
-- Just2 3

-- (Just2 2) >>= \x -> (Just2 1) >>= \y -> return (add x y)
-- Just2 3

-- do { x <- Just2 2; return x }
-- Just2 2

-- do { x <- Just2 2; return (inc x) }
-- Just2 3

-- do { x <- Just2 2; y <- Just2 1; return (add x y) }
-- Just2 3
