-- run using ghci. Type :reload to re-load the file

import Data.Char

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

-- pure id <*> x = x
-- pure (g x) = pure g <*> pure x
-- x <*> pure y = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- id "abc"
-- "abc"

-- pure id <*> "abc"
-- "abc"

-- id <$> "abc"
-- "abc"

double :: Int -> Int
double = (*2)

increment :: Int -> Int
increment = (+1)

-- double 3
-- 6

-- pure double <*> [3]
-- [6]

-- double <$> [3]
-- [6]

-- (increment . double) 3
-- 7

-- (increment . double) <$> [3]
-- [7]

-- double <$> Just 3
-- Just 6

-- (increment . double) <$> Just 3
-- Just 7

mult :: Int -> Int -> Int
mult x y = x * y

-- mult 2 3
-- 6

-- mult <$> [2] <*> [3]
-- [6]

-- increment <$> (mult <$> [2] <*> [3])
-- [7]

-- mult <$> Just 2 <*> Just 3
-- Just 6

-- increment <$> (mult <$> Just 2 <*> Just 3)
-- Just 7

-- Monads

data Expr = Val Int | Div Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- eval (Div (Val 6) (Val 3))
-- 2

-- eval (Div (Val 6) (Val 0))
-- *** Exception: divide by zero

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- safediv 6 3
-- Just 2
-- safediv 6 0
-- Nothing

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
  Nothing -> Nothing
  Just n -> case eval2 y of
    Nothing -> Nothing
    Just m -> safediv n m

-- eval2 (Div (Val 6) (Val 0))
-- Nothing

{-
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
  Nothing -> Nothing
  Just x -> f x
-}

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = eval3 x >>= \n ->
                    eval3 y  >>= \m ->
                    safediv n m

-- eval3 (Div (Val 6) (Val 3))
-- Just 2

eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = do
    n <- eval4 x
    m <- eval4 y
    safediv n m

-- eval4 (Div (Val 6) (Val 3))
-- Just 2

class Applicative2 m => Monad2 m where
  return2 :: a -> m a

  -- (>>=) :: m a -> (a -> m b) -> m b
  bind2 :: m a -> (a -> m b) -> m b

  return2 = pure2

instance Monad2 Maybe2 where
   
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- bind :: m a -> (a -> m b) -> m b
  bind2 Nothing2 _ = Nothing2
  bind2 (Just2 x) f = f x

safediv2 :: Int -> Int -> Maybe2 Int
safediv2 _ 0 = Nothing2
safediv2 n m = Just2 (n `div` m)

-- safediv2 6 3
-- Just2 2
-- safediv2 6 0
-- Nothing2

div_1 :: Int -> Maybe2 Int
div_1 x = Just2 (x `div` 1)

-- div_1 3
-- Val 3

eval5 :: Expr -> Maybe2 Int
eval5 (Val n) = Just2 n
eval5 (Div x y) = bind2 (eval5 x) (\n ->
                    bind2 (eval5 y) (\m ->
                    safediv2 n m))

-- eval5 (div_1 3)
-- Just2 3

-- bind :: m a -> (a -> m b) -> m b
-- bind2 (Just2 3) div_1
-- Just2 3

-- bind2 (eval5 (Val 3)) div_1
-- Just2 3

-- eval5 (Div (Val 6) (Val 3))
-- Just2 3
-- eval5 (Div (Val 6) (Val 0))
-- Nothing2

{-
instance Monad Maybe where
  -- (>>=) :: m a -> (a -> m b) -> m b
  Nothing >>=  _ = Nothing
  (Just x) >>= f = f x

instance Monad [] where
  -- (>>=) :: m a -> (a -> m b) -> m b
  xs >>= f = [y | x <- xs, y <- f x]
-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x <- xs, y <- ys]

-- pairs [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]

pairs2 :: [a] -> [b] -> [(a,b)]
pairs2 xs ys = xs >>= \x ->
  ys >>= \y ->
  [(x,y)]

-- pairs2 [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]

pairs3 :: [a] -> [b] -> [(a,b)]
pairs3 xs ys = do
  x <- xs
  y <- ys
  return (x,y)

-- pairs3 [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]

-- [1] >>= \x -> [x]
-- [1]

-- [1] >>= \x -> [2] >>= \y -> [(x,y)]
-- [(1,2)]

-- [1,2] >>= \x -> [3,4] >>= \y -> [(x,y)]
-- [(1,3),(1,4),(2,3),(2,4)]

-- do { x <- [1]; return x}
-- [1]

-- do { x <- [1]; y <- [2]; return (x,y) }
-- [(1,2)]

-- do { x <- [1,2]; y <- [3,4]; return (x,y) }
-- [(1,3),(1,4),(2,3),(2,4)]

-- State monad

type State = Int

-- ST - State Transformer

-- Generic functions

mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2 f [] = return []
mapM2 f (x:xs) = do
  y <- f x
  ys <- mapM2 f xs
  return (y:ys)

-- isDigit '1'
-- True
-- isDigit 'a'
-- False

-- digitToInt '1'
-- 1

conv :: Char -> Maybe Int

conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- conv '1'
-- Just 1

-- conv 'a'
-- Nothing

-- mapM conv "1234"
-- Just [1,2,3,4]

-- mapM conv "123a"
-- Nothing

filterM2 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM2 p [] = return []
filterM2 p (x:xs) = do
  b <- p x
  ys <- filterM2 p xs
  return (if b then x:ys else ys)

-- filterM2 (\x -> [True,False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- concat [[1], [2,3], []]
-- [1,2,3]

concat2 :: [[a]] -> [a]
concat2 [[]] = []
concat2 [x] = x
concat2 (x:xs) = x ++ concat2 xs

-- concat2 [[1], [2,3], []]
-- [1,2,3]

join :: Monad m => m (m a) -> m a
join mmx = do
  mx <- mmx
  x <- mx
  return x

-- join [[1], [2,3], []]
-- [1,2,3]

-- join (Just (Just 1))
-- Just 1

-- join (Just Nothing)
-- Nothing

-- join Nothing
-- Nothing

-- Monad laws


