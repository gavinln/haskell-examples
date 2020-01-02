-- run using ghci. Type :reload to re-load the file

-- Part 7: Higher-order functions

add :: Int -> Int -> Int
add x y = x + y

-- add 3 4
-- 7

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

-- add2 3 4

-- twice applies a function twice

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- twice (*2) 3
-- 12

-- twice reverse [1,2,3]
-- [1,2,3]

-- map2 applies a function to all elements of a list
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = [f x | x <- xs]

-- map2 (+1) [1,2,3]
-- [2,3,4]

-- map2 even [1,2,3,4]
-- [False,True,False,True]

-- map2 reverse ["abc", "def", "ghi"]
-- ["cba","fed","ihg"]

-- increments each number in a list of lists
-- map2 (map2 (+1)) [[1,2,3],[4,5]]
-- [[2,3,4],[5,6]]

map3 :: (a->b) -> [a] -> [b]
map3 f [] = []
map3 f (x:xs) = f x : map3 f xs

-- map3 (+1) [1,2,3,4]
-- [2,3,4,5]

-- filter2 selects all elemnts of a list that satisfy a predicate

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [x | x <- xs, p x]

-- filter2 even [1..10]
-- [2,4,6,8,10]

-- filter2 (>5) [1..10]
-- [6,7,8,9,10]

-- filter2 (/=' ') "abc def ghi"
-- "abcdefghi"

sum_sqr_even :: [Int] -> Int
sum_sqr_even ns = sum (map (^2) (filter even ns))

-- sum_sqr_even [1..5]
-- 20

-- all elements satisfy a predicate
-- all even [2,4,6,8]
-- True

-- any elements satisfy a predicate
-- any odd [2,4,6,8]
-- False

-- select elements if they satisfy a predicate
-- takeWhile even [2,4,6,8]
-- [2,4,6,8]

-- remove elements if they satisfy a predicate
-- dropWhile odd [1,3,5,6,7,8]
-- [6,7,8]

-- The foldr (fold right) function

-- Similar pattern
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

product2[] = 1
product2 (x:xs) = x + product2 xs

or2 [] = False
or2 (x:xs) = x || or2 xs

and2 [] = True
and2 (x:xs) = x || and2 xs

sum3 :: Num a => [a] -> a
sum3 = foldr (+) 0 

product3 :: Num a => [a] -> a
product3 = foldr (*) 1 

or3 :: [Bool] -> Bool
or3 = foldr (||) False

and3 :: [Bool] -> Bool
and3 = foldr (&&) True

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

-- foldr (+) 0 (1:(2:(3:[])))
-- 1 + (2 + (3 + 0))
-- 6
-- foldr (*) 1 (1:(2:(3:[])))
-- 1 * (2 * (3 * 1))
-- 6

length2 :: [a] -> Int
length2 [] = 0
length2 (x:xs) = 1 + length2 xs

-- 1 : (2 : (3 : []))
-- 1 + (1 + (1 + 0))

length3 :: [a] -> Int
length3 = foldr (\_ y -> 1 + y) 0

-- length2 [1,2,3]

-- length3 [1,2,3]

reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2(xs) ++ [x]

-- 1 : (2 : (3 : []))
-- (([] ++ [3]) ++ [2]) ++ [1]

-- reverse2 [1,2,3]
-- [3,2,1]

-- snoc - cons backward
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- snoc [1,2,3]

reverse3:: [a] -> [a]
reverse3 = foldr snoc []

-- reverse3 [1,2,3]
-- [3,2,1]

sum_left :: Num a => a -> [a] -> a
sum_left initial [] = initial
sum_left initial (x:xs) = sum_left (initial + x) xs

-- sum_left 0 [1,2,3]
-- 6

sum4 :: Num a => [a] -> a
sum4 = foldl (+) 0

product4 :: Num a => [a] -> a
product4 = foldl (*) 1

or4 :: [Bool] -> Bool
or4 = foldl (||) False

and4 :: [Bool] -> Bool
and4 = foldl (&&) True

-- sum4 [1,2,3,4]
-- 10
-- product4 [1,2,3,4]
-- 24
-- or4 [True,False,True]
-- True
-- and4 [True,False,True]
-- False

length4 :: [a] -> Int
length4 = foldl (\n _ -> n + 1) 0

reverse4 :: [a] -> [a]
reverse4 = foldl (\xs x -> x:xs) []

-- length4 [1,2,3,4]
-- 4

-- reverse4 [1,2,3,4]
-- [4,3,2,1]

foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f v [] = v
foldl2 f v (x:xs) = foldl2 f (f v x) xs

-- composition operator (.) returns a function

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

twice2 f x = f (f x)

-- twice2 (*2) 3
-- 12

twice3 f x = compose f f x

-- twice3 (*2) 3
-- 12

sum_sqr_even2 = sum . map (^2) . filter even

-- sum_sqr_even2 [1..5]
-- 20

-- id function returns its argument unchanged
id2 :: a -> a
id2 = \x -> x

-- compose a list of functions

compose2 :: [a -> a] -> (a -> a)
compose2 = foldr (.) id

-- Excercises
-- Ex 1. List comprehensions using map and filter

-- [x ^ 2| x <- [1..10], odd x]
-- [1,9,25,49,81]

-- map (^2) (filter odd [1..10])
-- [1,9,25,49,81]

-- Ex 2. Define higer-order functions on list

-- Ex 2.a. All elements of a list satisfies a predicate

-- all even [2,4,6]
-- True
-- all even [3,4,6]
-- False

all2 :: (a -> Bool) -> [a] -> Bool
all2 f [] = True
all2 f (x:xs) = (f x) && all2 f xs

-- all2 even [2,4,6]
-- True
-- all2 even [3,4,6]
-- False

-- (f 2 && (f 4 && (f 6 && f [])

all3 :: (a -> Bool) -> [a] -> Bool
all3 f = foldr apply_and True
         where apply_and x y = f x && y

-- all3 even [2,4,6]
-- True
-- all3 even [3,4,6]
-- False

-- Ex 2.b. Any elements of a list satisfies a predicate

-- any even [1,3,5]
-- False
-- any even [2,3,5]
-- True

any2 :: (a -> Bool) -> [a] -> Bool
any2 f [] = False
any2 f (x:xs) = (f x) || any2 f xs

-- any2 even [1,3,5]
-- False
-- any2 even [2,3,5]
-- True

-- (f 1 || (f 3 || (f 5 || f [])

any3 :: (a -> Bool) -> [a] -> Bool
any3 f = foldr apply_or False
         where apply_or x y = f x || y

-- any3 even [1,3,5]
-- False
-- any3 even [2,3,5]
-- True

-- Ex 2.c. Select elements while they satisfy a predicate

-- takeWhile even [2,4,5,6]
-- [2,4]

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (x:xs) | (f x)     = [x] ++ takeWhile2 f xs
                    | otherwise = []

-- takeWhile2 even [2,4,5,6]
-- [2,4]

-- f 2 : (f 4 : (f 5 : (f 6 : [])))
-- ([2] ++ ([4] ++ ([])

takeWhile3 :: (a -> Bool) -> [a] -> [a]
takeWhile3 f = foldr test_concat_if []
         where test_concat_if x y = if f x then [x] ++ y else []

-- takeWhile3 even [2,4,5,6]
-- [2,4]

-- Ex 2.d. Remove elements while they satisfy a predicate

-- dropWhile even [2,4,5,6]
-- [5,6]

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f (x:xs) | f x       = dropWhile2 f xs
                    | otherwise = x:xs

-- dropWhile2 even [2,4,5,6]
-- [5,6]

-- (((([] ++ f 2) ++ f 4) ++ f 5) ++ f 6
-- ((([] ++ []) ++ []) + [5,6]

-- NOT CORRECT
dropWhile3 :: (a -> Bool) -> [a] -> [a]
dropWhile3 f = foldl test_drop_if []
         where test_drop_if x y = if f y then x else x ++ [y]

-- dropWhile3 even [2,4,5,6]
-- [5]

-- :reload
