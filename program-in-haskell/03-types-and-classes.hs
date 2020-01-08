-- run using ghci. Type :reload to re-load the file

-- Part 3: Types and classes

False :: Bool

True :: Bool

not 1
-- has an error

not False
-- True

1 `div` 0
-- raises a divide by zero exception

:type not
-- not :: Bool -> Bool

:type False
-- False :: Bool

:type not False
-- not False :: Bool

{-
 - Basic types
 -}

-- Char type
:type 'A'
:type '_'
:type '\n'

-- Int types
:type 300
:type -20
:type 0
:type 2^60

-- Integer types
:type 2 ^ 128 :: Integer

-- Float types
:type -12.34
:type 1.0
:type 3.1415

-- Double types
:type 1.4142 :: Double
:type sqrt 2 :: Double

-- List types
:type [False,True,False]
-- [Bool]

:type ['a','b','c','d']
-- [Char]

:type ["one","two","three"] :: [String]
-- [String]

length []
-- 0

length [[]]
-- 1

:type [['a','b'],['c']]
-- [[Char]]

-- Tuples
-- finite sequence of components of different types

:type (False,True)
-- (Bool, Bool)

:type (False,'a',True)
-- (Bool, Char, Bool)

-- empty tuple with arity (length) zero

:type ()

-- not a tuple

:type (3)

:type ('a',(False,'b'))
-- (Char, (Bool, Char))

:type (['a'],[False,True])
-- ([Char], [Bool])

:type [('a',False),('b',True)]
-- [(Char, Bool)]

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

