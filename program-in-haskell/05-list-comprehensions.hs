-- run using ghci. Type :reload to re-load the file

-- Part 5: List comprehensions

-- In Mathematics a comprehension is used to create a new set from existing
-- sets

-- [x ^ 2 | x <- [1..5]]

-- [(x,y) | y <- [1,2,3], x <- [4,5]]

-- [(x,y) | x <- [1..3], y <- [x..3]]

concat2 :: [[a]] -> [a]
concat2 xss = [x | xs <- xss, x <- xs]

-- concat2 [[1],[2],[3, 4]]
-- [1,2,3,4]

-- first element of each tuple

firsts :: [(a, b)] -> [a]
firsts pairs = [x | (x,_) <- pairs]

-- firsts [(2, 3), (1, 2), (1, 2)]
-- [2,1,1]

length2 :: [a] -> Int
length2 xs = sum [1 | _ <- xs]

-- length2 [2, 3, 1]
-- 3

-- Guards
-- guards are used to filter the values

evens :: Int -> [Int]
evens n = [x | x <- [1..n], even x] 

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- factors 7
-- [1,7]

-- factors 15
-- [1,3,5,15]

prime :: Int -> Bool
prime n = factors n == [1,n]

-- prime 5
-- True

-- prime 15
-- False

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- primes 40
-- [2,3,5,7,11,13,17,19,23,29,31,37]

find :: Eq a => a -> [(a,b)] -> [b]
find k pairs = [val | (key,val) <- pairs, k == key]

-- find 'b' [('a',1), ('b',2), ('c',3), ('b',4)]
-- [2,4]

-- Zip function

-- zip ['a','b','c'] [1,2,3]
-- [('a',1),('b',2),('c',3)]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]

-- checks if items are sorted
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- sorted [1, 2, 3]
-- True

-- sorted [1, 4, 3]
-- False

-- positions where an item exist in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (y, i) <- zip xs [0..], y == x]

-- positions 1 [2, 1, 3, 4, 1]
-- [1,4]

-- String comprehensions

-- strings are list of characters
-- "abcde" !! 2
-- 'c'

-- take 3 "abcde"
-- "abc"

-- length "abcde"
-- 5

-- zip "abc" [1,2,3,4]
-- [('a',1),('b',2),('c',3)]

-- get the number of lower case characters
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- lowers "Haskell"
-- 6

-- count number of specified Char in a String
counts :: Char -> String -> Int
counts x xs = length [y | y <- xs, x == y]

-- counts 's' "Mississippi"
-- 4

-- Exercises
-- Ex. 1 - sum of squares

-- sum [x^2 | x <- [1..100]]
-- 338350

-- Ex. 2 - coordinate grid

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x, y) | x <- [0..x], y <- [0..y]]

-- grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

-- Ex. 3 - square excluding diagonal

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

-- Ex 4. - duplicate library function replicate

replicate2 :: Int -> a -> [a]
replicate2 n a = [a | b <- [0..n - 1]]

-- replicate2 3 True
-- [True,True,True]

-- Ex 5. - Pythagorean triplet x^2 + y^2 = z^2

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [a..n], y <- [a..n], z <- [a..n], x^2 + y^2 == z^2]
          where a = 1

-- pyths 10 
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

-- Ex 6. - Perfect numbers are the sum of their factors
-- 6 = 1 + 2 + 3

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- perfects 500
-- [6,28,496]

-- :reload
