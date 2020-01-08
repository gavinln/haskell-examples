-- run using ghci. Type :reload to re-load the file

-- Part 3: Types and classes - Exercises

-- Types of
-- :type ['a','b','c']
-- [Char]
--
-- :type ('a','b','c')
-- (Char,Char,Char)
--
-- :type [(False,'0'),(True,'1')]
-- [(Bool,Char)]
--
-- :type ([False,True],['0','1'])
-- ([Bool],[Char])
--
-- :type [tail,init,reverse]
-- [[a] -> [a]]

-- Definitions that have types
-- bools :: [Bool]
-- :type [False,True]
--
-- nums :: [[Int]]
-- :type [[2,3],[1,2,3]]
--
-- add :: Int -> Int -> Int -> Int
add' x y z = x + y + z
-- :type add'

-- copy :: a -> (a,a)
copy a = (a,a)
-- :type copy

-- apply :: (a -> b) -> a -> b
apply f x = f x
-- :type apply

second xs = head (tail xs)
-- :type second
-- second :: [a] -> a

swap (x,y) = (y,x)
-- :type swap
-- swap :: (a,b) -> (b,a)

pair x y = (x,y)
-- :type pair
-- pair :: a -> b -> (a, b)

double x = x*2
-- :type double
-- double :: Num a => a -> a

palindrome xs = reverse xs == xs
-- :type palindrome
-- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
-- :type twice
-- twice :: (a -> a) -> a -> a

