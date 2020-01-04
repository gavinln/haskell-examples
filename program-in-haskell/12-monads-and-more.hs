-- run using ghci. Type :reload to re-load the file

-- Part 12: Monads and more

-- 12.1 Functors

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns

-- inc [1,2,3,4]
-- [2,3,4,5]
-- sqr [1,2,3,4]
-- [1,4,9,16]

inc2 = map (+1)
sqr2 = map (^2)

-- Functors are the types of classes that support mapping functions

class Functor2 f where
  fmap2 :: (a -> b) -> f a -> f b

{-
-- List is a functor
instance Functor [] where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = map
-}

data Maybe2 a = Nothing2 | Just2 a deriving Show

instance Functor2 Maybe2 where
  -- fmap :: (a -> b) -> f a -> f b

  fmap2 _ Nothing2  = Nothing2
  fmap2 g (Just2 x) = Just2 (g x)

-- fmap (*2) [1,2,3,4]

-- fmap2 (*2) Nothing2
-- Nothing2

-- fmap2 (*2) (Just2 1)
-- Just2 2

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor2 Tree where
  -- fmap :: (a -> b) -> f a -> f b

  fmap2 g (Leaf x) = Leaf (g x)
  fmap2 g (Node l r) = Node (fmap2 g l) (fmap2 g r)

-- fmap2 length (Leaf "abc")
-- Leaf 3

-- fmap2 even (Node (Leaf 1) (Leaf 2))
-- Node (Leaf False) (Leaf True)

instance Functor2 IO where
  -- fmap :: (a -> b) -> f a -> f b

  fmap2 g mx  = do
    x <- mx
    return (g x)

-- fmap2 show (return True)
-- "True"

-- define inc using functor fmap

inc3 :: Functor2 f => f Int -> f Int
inc3 = fmap2 (+1)

-- inc3 (Just2 1)
-- Just2 2

-- inc [1,2,3,4]
-- [2,3,4,5]

-- inc3 (Node (Leaf 1) (Leaf 2))
-- Node (Leaf 2) (Leaf 3)

-- Functor laws

-- fmap id = id
-- fmap (g . h) = fmap g . fmap h

-- fmap id [1,2,3,4]
-- [1,2,3,4]

-- fmap (not . env) [1,2]

-- 12.2 Applicatives

-- 12.3 Monads

-- :reload
-- Ok, one module loaded.
