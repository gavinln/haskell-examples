-- run using ghci. Type :reload to re-load the file

import Data.Char

-- Part 12: Monads and more - Exercises

-- fmap (+1) [1..3]
-- [2,3,4]

-- Exercise 1

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

make_tree1 = Node Leaf 1 (Node (Node Leaf 3 Leaf) 2 Leaf)

-- make_tree1
-- Node Leaf 1 (Node (Node Leaf 3 Leaf) 2 Leaf)

instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b

  fmap _ Leaf = Leaf
  fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

-- fmap (+1) make_tree1
-- Node Leaf 2 (Node (Node Leaf 4 Leaf) 3 Leaf)

-- Exercise 2

{-
instance Functor ((->) a) where
  -- fmap :: (b-> c) -> (a -> b) -> (a -> c)
  fmap = (.)
-}


-- Exercise 3
{-
instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const
  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)
-}

data Maybe2 a = Nothing2 | Just2 a deriving Show

class Functor2 f where
  fmap2 :: (a -> b) -> f a -> f b

instance Functor2 Maybe2 where
  -- fmap :: (a -> b) -> f a -> f b

  fmap2 _ Nothing2  = Nothing2
  fmap2 g (Just2 x) = Just2 (g x)

inc x = x + 1
double x = x * 2
add x y = x + y

-- fmap inc [1,2]
-- [2,3]
-- fmap double [1,2]
-- [2,4]
-- fmap (inc . double) [1,2]
-- [3,5]
-- fmap (double . inc) [1,2]
-- [4,6]

class Functor2 f => Applicative2 f where
  pure2 :: a -> f a
  apply2 :: f (a -> b) -> f a -> f b

  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative2 Maybe2 where
  -- pure :: a -> Maybe a
  pure2 = Just2

  -- apply2 :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
  apply2 (Just2 g) mx = fmap2 g mx

-- apply2 (pure2 inc) (Just2 2)
-- Just2 3
-- apply2 (pure2 inc) Nothing2
-- Nothing2

