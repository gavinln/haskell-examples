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

inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = x * 2

add :: Num a => a -> a -> a
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

  -- (>>=) :: m a -> (a -> m b) -> m b
  bind2 :: m a -> (a -> m b) -> m b

  return2 x = pure2 x

instance Monad2 Maybe2 where
   
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- bind :: m a -> (a -> m b) -> m b
  bind2 Nothing2 _ = Nothing2
  bind2 (Just2 x) f = f x

inc_maybe2 :: Num a => a -> Maybe2 a
inc_maybe2 x = Just2 (x + 1)

-- bind2 (Just2 2) inc_maybe2
-- Just2 3

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

-- fmap inc (Z [1..3])
-- Z [2,3,4]

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x] 

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x| g <- gs, x <- xs]

-- inc <$> (Z [1..3])
-- Z [2,3,4]
-- (pure inc) <*> (Z [1..3])
-- Z [2,3,4]

