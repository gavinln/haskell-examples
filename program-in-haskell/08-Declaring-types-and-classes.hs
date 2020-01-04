-- run using ghci. Type :reload to re-load the file

-- Part 8: Declaring types and classes

type String2 = [Char]

length2 :: String2 -> Int
length2 = length

-- length2 "abc"
-- 3

-- Type declarations

type Pos = (Int,Int)

type Trans = Pos -> Pos

type Pair a  = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- find 2 [(1,'a'),(2,'b'),(3,'c'),(2,'d')]
-- 'b'

-- Data declarations

data Fruit = Apple | Banana

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

-- move (rev East) (move East (2,3))
-- (2,3)

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square a = Rect a a

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- area (square 3)
-- 9.0

data Maybe2 a = Nothing2 | Just2 a

safediv :: Int -> Int -> Maybe2 Int
safediv _ 0 = Nothing2
safediv m n = Just2 (m `div` n)

-- safediv 5 2
-- safediv 5 0

safehead :: [a] -> Maybe2 a
safehead [] = Nothing2
safehead xs = Just2 (head xs)

-- safehead [5,2]
-- safehead []

-- newtype declarations

newtype Nat2 = N Int

-- Recursive types

-- Nat is a natural number representing integers starting with zero
-- Succ represents the successor function (1+)
-- Succ (Succ (Succ Zero)) represents 1 + (1 + (1 + 0) = 3

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- (nat2int . Succ . int2nat) 3
-- 4

add :: Nat -> Nat -> Nat
add m n = int2nat(nat2int m + nat2int n)

-- nat2int (add (int2nat 3) (int2nat 4))
-- 7

add2 :: Nat -> Nat -> Nat
add2 Zero n = n
add2 (Succ m) n = Succ (add2 m n)

-- nat2int (add2 (Succ (Succ Zero)) (Succ Zero))
-- 3

-- declaring custom lists

data List a = Nil | Cons a (List a)

length3 :: List a -> Int
length3 Nil = 0
length3 (Cons _ xs) = 1 + length3 xs

-- length3 Nil
-- 0
-- length3 (Cons 3 Nil)
-- 1
-- length3 (Cons 3 (Cons 2 Nil))
-- 2

-- declaring trees

data Tree a = Leaf a | Node (Tree a) a (Tree a)

sample_tree :: Tree Int
sample_tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5
  (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- occurs 3 sample_tree
-- True
-- occurs 2 sample_tree
-- False

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- flatten sample_tree
-- [1,3,4,5,6,7,9]

-- sorted tree
occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y)                  = x == y
occurs2 x (Node l y r) | x == y     = True
                       | x < y      = occurs2 x l
                       | otherwise  = occurs2 x r

-- occurs2 3 sample_tree
-- True
-- occurs2 2 sample_tree
-- False

-- Class and instance declarations

{-
class Eq a where
  (==),(/=) :: a -> a -> Bool
  x /= y = not (x == y )

instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False

class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max             :: a -> a -> a

  min x y | x <= y    = x
          | otherwise = y

  max x y | x >= y    = x
          | otherwise = y

instance Ord Bool where
  False < True = True
  _     < _    = False

  b <= c = (b < c) | (b == c)
  b > c  = c < b
  b >= c = c <= b
-}

-- Derived instances

data Bool2 = False2 | True2 deriving (Eq, Ord, Show, Read)

-- False2 == False2
-- True
-- False2 < False2
-- False
-- show False2
-- "False2"
-- read "False2" :: Bool2
-- False2

-- Exercies

-- Ex 1. Define a multiplication function for Nat

mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ m) n = add2 (mult m n) n

two = Succ (Succ Zero)

three = Succ two

-- nat2int (mult Zero two)
-- 0
-- nat2int (mult three two)
-- 6

-- Ex 2. Use a custom Ordering type

data Ordering3 = LT3 | EQ3 | GT3 deriving (Eq, Show)

compare3 :: Ord a => a -> a -> Ordering3
compare3 a b | a < b     = LT3
             | a == b    = EQ3
             | otherwise = GT3

-- compare3 3 4
-- LT3

occurs3 :: Ord a => a -> Tree a -> Bool
occurs3 x (Leaf y)                  = x == y
occurs3 x (Node l y r) | compare3 x y == EQ3     = True
                       | compare3 x y == LT3     = occurs3 x l
                       | otherwise               = occurs3 x r

-- occurs3 3 sample_tree
-- True
-- occurs3 2 sample_tree
-- False

-- Ex 3. Binary tree

-- nodes do not have data only leaves
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Show)

sample_tree2_bal = Node2 (Leaf2 4) (Node2 (Leaf2 5) (Leaf2 6))

sample_tree2_unbal = Node2 (Leaf2 4) (Node2 (Leaf2 5) (Node2 (Leaf2 6) (Leaf2 7)))

-- sample_tree2_bal
-- Node2 (Leaf2 4) (Node2 (Leaf2 5) (Leaf2 6))

-- sample_tree2_unbal
-- Node2 (Leaf2 4) (Node2 (Leaf2 5) (Node2 (Leaf2 6) (Leaf2 7)))

leaves :: Tree2 a -> Int
leaves (Leaf2 x) = 1
leaves (Node2 x y) = leaves x + leaves y

-- leaves sample_tree2_bal
-- 3
-- leaves sample_tree2_unbal
-- 4

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 x y) | diff_leaves == 0  = True
                     | diff_leaves == 1  = True
                     | diff_leaves == -1 = True
                     | otherwise         = False
      where diff_leaves = (leaves x) - (leaves y)

-- balanced (Leaf2 2)
-- True
-- balanced sample_tree2_bal
-- True
-- balanced sample_tree2_unbal
-- False

sample_list = [1,2,4,5,7]

-- sample_list

halve :: [a] -> ([a],[a])
halve x = (take part x, drop part x)
  where part = (length x) `div` 2

-- halve sample_list
-- ([1,2],[4,5,7])

-- build a balanced tree
balance2 :: [a] -> Tree2 a
balance2 [x] = Leaf2 x
balance2 xs = Node2 (balance2 first) (balance2 second)
    where (first,second) = halve(xs)

-- balance2 [1]
-- Leaf2 1
-- balance2 [1,2]
-- Node2 (Leaf2 1) (Leaf2 2)
-- balance2 [1,2,3,4,5]
-- Node2 (Node2 (Leaf2 1) (Leaf2 2)) (Node2 (Leaf2 3) (Node2 (Leaf2 4) (Leaf2 5)))

-- :reload
