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

-- fmap (not . even) [1,2]
-- [True,False]

-- 12.2 Applicatives

-- fmap_0 :: a -> f a
-- fmap_1 :: (a -> b) -> f a -> f b
-- fmap_2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap_3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-- fmap_2 (+) (Just 1) (Just 2)
-- Just 3

-- pure2 converts value o type a into structure of type f a
-- pure :: a -> f a

-- <*> is a generalized form of function application where the function
-- parameters and return value are contained in f structures
-- (<*>) :: f (a -> b) -> f a -> f b

-- fmap_0 :: a -> f a
-- fmap_0 = pure

-- fmap_1 :: (a -> b) -> f a -> f b
-- fmap_1 g x = pure g <*> x

-- fmap_2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap_2 g x y = pure g <*> x <*> y

-- fmap_3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- fmap_3 g x y z = pure g <*> x <*> y <*> z

class Functor2 f => Applicative2 f where
  pure2 :: a -> f a
  apply2 :: f (a -> b) -> f a -> f b

instance Applicative2 Maybe2 where
  -- pure :: a -> Maybe a
  pure2 = Just2

  -- apply2 :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
  apply2 Nothing2 _ = Nothing2
  apply2 (Just2 g) mx = fmap2 g mx

-- apply2 (pure2 (+1)) (Just2 1)
-- Just2 2

-- apply2 (apply2 (pure2 (+)) (Just2 1)) (Just2 2)
-- Just2 3

-- pure (+1) <*> Just 1
-- Just 2

-- pure (+) <*> Just 1 <*> Just 2
-- Just 3

{-

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]
-}

-- pure (+1) <*> [1,2,3]
-- [2,3,4]

-- pure (+) <*> [1] <*> [2]
-- [3]

-- pure (*) <*> [1,2] <*> [3,4]
-- [3,4,6,8]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <-xs, y <- ys]

-- prods [1,2] [3,4]
-- [3,4,6,8]

prods2 :: [Int] -> [Int] -> [Int]
prods2 xs ys = pure (*) <*> xs <*> ys

-- prods2 [1,2] [3,4]
-- [3,4,6,8]

instance Applicative2 IO where
  -- pure :: a -> IO a
  pure2 = return

  -- apply2 :: f (a -> b) -> f a -> f b
  apply2 mg mx = do
    g <- mg
    x <- mx
    return (g x)

-- apply2 (pure2 show) (return True)
-- "True"

-- pure show <*> (return 1)
-- "1"

-- pure (++) <*> getLine <*> getLine

-- apply2 (apply2 (pure2 (++)) getLine) getLine

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

-- getChars 3

-- convert a list of applicatives to a single applicative and a list of results
sequenceA2 :: Applicative2 f => [f a] -> f [a]
sequenceA2 [] = pure2 []
sequenceA2 (x:xs) = apply2 (apply2 (pure2 (:)) x) (sequenceA2 xs)

getCharTwice = sequenceA2 (replicate 2 getChar)

-- getCharTwice

getChars2 :: Int -> IO String
getChars2 n = sequenceA (replicate n getChar)

-- getChars2 3

-- Applicative laws

-- Monads
