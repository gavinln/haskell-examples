-- run using ghci. Type :reload to re-load the file

-- Part 6: Recursive functions

-- factorial
fac :: Int -> Int
fac n = product [1..n]

-- fac 6
-- 720

fac2 :: Int -> Int
fac2 0 = 1
fac2 n = n * fac (n - 1)

-- fac2 6
-- 720

product2 :: Num a => [a] -> a
product2 []     = 1
product2 (x:xs) = x * product xs

-- product [1,2,3,4,5,6]
-- 720
-- product2 [1,2,3,4,5,6]
-- 720

length2 :: [a] -> Int
length2 []     = 0
length2 (_:xs) = 1 + length2 xs

-- length [1,2,3,4,5,6]
-- 6
-- length2 [1,2,3,4,5,6]
-- 6

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

-- reverse [1,2,3,4,5,6]
-- [6,5,4,3,2,1]
-- reverse2 [1,2,3,4,5,6]
-- [6,5,4,3,2,1]

-- append operator ++ can be defined using recursive functions

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x:(append xs ys) 

-- append [1,2,3] [4,5]
-- [1,2,3,4,5]

-- insert into a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x []   = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

-- insert 3 [1,2,3,4,5]
-- [1,2,3,3,4,5]

-- insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- isort [3,1,3,4,5,2]
-- [1,2,3,3,4,5]

-- Multiple argument functions defined recursively

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] _  = []
zip2 _  [] = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

-- zip2 "abc" [1,2,3,4]
-- [('a',1),('b',2),('c',3)]

drop2 :: Int -> [a] -> [a]
drop2 0 xs     = xs
drop2 _ []     = []
drop2 n (_:xs) = drop2 (n - 1) xs

-- drop2 3 [1,2,3,4,5]
-- [4,5]

-- mutual recursion
-- a function is applied more than once in its own definion

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- fib 10
-- 55

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                smaller = [a | a <- xs, a <= x]
                larger  = [b | b <- xs, b > x]

-- qsort [1,3,2,4,3]
-- [1,2,3,3,4]

-- mutual recursion
-- two or more functions are defined recursively in terms of each other
-- 

is_even :: Int -> Bool
is_even 0 = True
is_even n = is_odd (n-1)

is_odd :: Int -> Bool
is_odd 0 = False
is_odd n = is_even (n-1)

-- is_even 23
-- False

-- is_odd 23
-- True

-- get_evens - select elements at even positions
get_evens :: [a] -> [a]
get_evens [] = []
get_evens (x:xs) = x : (get_odds xs)

-- get_odds - select elements at odd positions
get_odds :: [a] -> [a]
get_odds [] = []
get_odds (_:xs) = get_evens xs

-- get_evens "abcde"
-- "ace"
-- get_odds "abcde"
-- "bd"

-- Process
-- 1. Define the type
-- 2. Enumerate the cases
-- 3. Define the simple cases
-- 4. Define the other cases
-- 5. Generalise and simplify

-- Exercises
-- Ex 1. Add a guard to prohibit negative arguments to factorial

fac3 :: Int -> Int
fac3 n | n > 0 = n * fac3 (n - 1)

-- fac3 (-2)
-- *Main> -- fac3 (-2)

-- Ex 2. sumdown function adding values from number to zero
-- sumdown 3: 3 + 2 + 1 + 0

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- sumdown 3
-- 6

-- Ex 3. exponentiation operator ^ defined recursively
exp2 :: Int -> Int -> Int
exp2 a 0 = 1
exp2 a n = a * (exp2 a (n-1))

-- exp2 2 3
-- 8

-- Ex 4. Greatest common divisor - Euclid's algorithm

euclid :: Int -> Int -> Int
euclid x y | x == y    = x
           | x > y     = euclid (x - y) y
           | otherwise = euclid (y - x) x

-- euclid 6 27
-- 3

-- Ex 6. Recursive implementation
-- and: True if all logical values in a list are true - and2
and2 :: [Bool] -> Bool
and2 [b]  = b
and2 (x:xs)  = x && and2 xs

-- and2 [True,True,True]
-- True
-- and2 [True,False,True]
-- False

-- concat: concatenate a list of lists - concat2
concat2 :: [[a]] -> [a]
concat2 [[x]] = [x]
concat2 (x:xs) = x ++ (concat2 xs)

-- concat2 [[1], [2,3], [4]]

-- replicate: produce a list with n identical elements - replicate2

replicate2 :: Int -> a -> [a]
replicate2 0 a = []
replicate2 n a = [a] ++ replicate2 (n-1) a

-- replicate2 3 'c'
-- "ccc"

-- !!: select the nth element of a list - index

index :: [a] -> Int -> a
index (x:xs) 0    = x
index (x:xs) n = index xs (n-1)

-- index [0,1,2,3] 2
-- 2

-- index "abcdef" 2
-- 'c'

-- elem: True if element in list - elem2
elem2 :: Eq a => a -> [a] -> Bool
elem2 y (x:xs) | null xs && x /= y = False
               | null xs && x == y = True
               | otherwise = elem2 y xs

-- elem2 'o' "Hello"
-- True
-- elem2 'a' "Hello"
-- False

-- Ex 7. merge two sorted list to create a single sorted list

merge :: Ord a => [a] -> [a] -> [a]
merge x []    = x
merge [] y    = y
merge (x:xs) (y:ys) | x <= y = [x] ++ merge xs (y:ys)
                    | x > y  = [y] ++ merge (x:xs) ys

-- merge [2,5,6] [1,3,4,7]
-- [1,2,3,4,5,6,7]

-- Ex 8. merge sort using merge and halve function -- msort

halve :: [a] -> ([a],[a])
halve x = (take half x, drop half x)
          where half = length x `div` 2

-- halve "abcde"
-- ("ab","cde")
-- halve "abcdef"
-- ("abc","def")

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
           where (first,second) = halve xs

-- msort [2,1,3,5,4]
-- [1,2,3,4,5]

-- Ex 9.
-- sum of a list of numbers - sum2

sum2 :: [Double] -> Double
sum2 []     = 0
sum2 (x:xs) = x + sum2 xs

-- sum2 [1,2,3,4,5]
-- 15.0

-- take n elements from the start of a list - take2

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 n [] = []
take2 n (x:xs) = [x] ++ take2 (n-1) xs

-- take2 3 "abcde"
-- "abc"
-- take2 7 "abcde"
-- "abcde"

-- select the last element of a non-empty list - last2

last2 :: [a] -> a
last2 [a] = a
last2 (x:xs) = last2 xs 

-- last2 "abc"
-- 'c'

-- last2 "a"
-- 'a'

-- :reload
