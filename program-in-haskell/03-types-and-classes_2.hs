-- run using ghci. Type :reload to re-load the file

-- Part 3: Types and classes 2

-- Function types
-- :type not
-- not :: Bool -> Bool

-- :type even
-- even :: Integral a => a -> Bool

add :: (Int,Int) -> Int
add (x,y) = x + y

-- :type add
-- add :: (Int, Int) -> Int

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- :type zeroto
-- zeroto :: Int -> [Int]

-- Curried functions

add' :: Int -> (Int -> Int)
add' x y = x + y

-- :type add'
-- add' :: Int -> Int -> Int

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- :type mult

-- Function arrow -> in types associates to the right
-- mult :: Int -> Int -> Int - Int
-- mult :: Int -> (Int -> (Int -> Int))

-- Function arrow -> in function applications associates to the left
-- mult x y z
-- ((mult x) y ) z

-- Polymorphic types
-- length works for a list of integers/strings/functions
-- length [1,3,5,7]
-- length ["Yes","No"]
-- length [sin,cos,tan]

-- :type length

-- types of some functions in the standard prelude
-- :type fst
-- fst :: (a, b) -> a

-- :type head
-- head :: [a] -> a

-- :type take
-- take :: Int -> [a] -> [a]

-- :type zip
-- zip :: [a] -> [b] -> [(a, b)]

-- :type id
-- id :: a -> a

-- Overloaded types
-- 1 + 2
-- 1.0 + 2.0

-- :type (+)
-- (+) :: Num a => a -> a -> a

-- :type (*)
-- (*) :: Num a => a -> a -> a

-- :type negate
-- negate :: Num a => a -> a

-- :type abs
-- abs :: Num a => a -> a

-- Basic classes
-- A class is a collection of types that support certain overloaded operations
-- called methods

-- Equality types
-- :type (==)
-- (==) :: Eq a => a -> a -> Bool
-- :type (/=)
-- (/=) :: Eq a => a -> a -> Bool

-- Bool, Char, String, Int, Integer, Float, Double, list and tuple are
-- instances of Eq class
-- False == False
-- 3 == 5
-- 'a' == 'b'
-- "abc" == "abc"
-- [1,2] == [1,2,3]
-- ('a',False) == ('a',False)

-- Ord - ordered types
-- :type (<)
-- (<) :: Ord a => a -> a -> Bool
-- :type (<=)
-- (<=) :: Ord a => a -> a -> Bool
-- :type (>)
-- (>) :: Ord a => a -> a -> Bool
-- :type (>=)
-- (>=) :: Ord a => a -> a -> Bool
-- :type min
-- min :: Ord a => a -> a -> a
-- :type max
-- max :: Ord a => a -> a -> a

-- All basic types are instances of the Ord class
-- False < True
-- True

-- min 'a' 'b'
-- 'a'

-- "elegant" < "elephant"
-- True

-- [1,2,3] < [1,2]
-- False

-- ('a',2) < ('b',1)
-- True

-- ('a',2) < ('a',1)
-- False

-- Show - showable types
-- The class that contains types whose values can be converted into strings

-- :type show
-- show :: Show a => a -> String

-- show "False"
-- show 123

-- Read - redable types
-- types whose value can be converted from strings of characters

-- :type read
-- read :: Read a => String -> a
--
-- read "False" :: Bool
-- False

-- read "’a’" :: Char
-- ’a’

-- read "123" :: Int
-- 123

-- read "[1,2,3]" :: [Int]
-- [1,2,3]

-- read "(’a’,False)" :: (Char,Bool)
-- (’a’,False)

-- not (read "abc")
-- Causes an exception

-- Num numeric types
-- Class containing types whose values are numeric

-- (+) :: a -> a -> a
-- (-) :: a -> a -> a
-- (*) :: a -> a -> a
-- negate :: a-> a
-- abs :: a -> a
-- signum :: a-> a

-- signum 3
-- 1

-- signum (-3)
-- -1

-- Integral types
-- Class whose values are integers and support integer division and integer
-- remainder

-- :type div
-- div :: Integral a => a -> a -> a

-- :type mod
-- mod :: Integral a => a -> a -> a

-- 7 `div` 2
-- 7 `mod` 2

-- Fractional types
-- Class that contains types whose instances have non-integral values and
-- support franctional division and reciprocation

-- :type (/)
-- (/) :: Fractional a => a -> a -> a

-- :type recip
-- recip :: Fractional a => a -> a

-- Exercises
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

