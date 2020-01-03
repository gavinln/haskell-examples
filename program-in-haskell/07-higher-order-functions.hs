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

-- takeWhile2 even [2,4,5,6,7]
-- [2,4]

-- f 2 : (f 4 : (f 5 : (f 6 : (7 : [])))
-- ([2] ++ ([4] ++ ([])

takeWhile3 :: (a -> Bool) -> [a] -> [a]
takeWhile3 f = foldr test_concat_if []
         where test_concat_if x y = if f x then [x] ++ y else []

-- takeWhile3 even [2,4,5,6,7]

-- Ex 2.d. Remove elements while they satisfy a predicate

-- dropWhile even [2,4,5,6,7]
-- [5,6,7]

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f (x:xs) | f x       = dropWhile2 f xs
                    | otherwise = x:xs

-- dropWhile2 even [2,4,5,6,7]
-- [5,6,7]

-- ((((([] ++ f 2) ++ f 4) ++ f 5) ++ f 6) ++ f 7
-- ((([] ++ []) ++ []) + [5,6,7]

dropWhile3 :: (a -> Bool) -> [a] -> [a]
dropWhile3 f xs = foldl test_drop_if [] xs
         where test_drop_if x y | length x == 0 && f y = []
                                | otherwise            = x ++ [y]

-- dropWhile3 even [2,4,5,6,7]
-- [5,6,7]

-- Ex 3. Define map f and filter p using foldr

-- 1 : (2 : (3 : (4 : [])))

map4 :: (a -> b) -> [a] -> [b]
map4 f xs = foldr apply_cons [] xs
            where apply_cons x y = f x : y

-- map4 (^2) [1,2,3,4]
-- [1,4,9,16]

filter4 :: (a -> Bool) -> [a] -> [a]
filter4 f xs = foldr filter_cons [] xs
               where filter_cons x y | f x       = x : y
                                     | otherwise = y
-- filter4 even [1,2,3,4]
-- [2,4]

-- Ex 4. Using foldl convert a decimal number to an integer

-- 2 # (3 # (4 + # (5 + 0)
-- 2 + 10 * (3 + 10 * (4 + 10 * (5 + 0)

dec2int' :: [Int] -> Int
dec2int' [] = 0
dec2int' (x:xs) = (x + dec2int' xs) * 10

dec2int :: [Int] -> Int
dec2int xs = dec2int' (reverse xs) `div` 10

-- dec2int [2,3,4,5]
-- 2345

-- ((2 # 3) # 4) # 5
-- (((0 # 2) # 3) # 4) # 5
-- (((0 * 10 + 2) * 10 + 3) * 10 + 4) * 10 + 5

dec2int2 :: [Int] -> Int
dec2int2 xs = foldl add_mult 0 xs
              where add_mult x y = x * 10 + y

-- dec2int2 [2,3,4,5]
-- 2345

-- Ex. 5 - curry and uncurry

add_pair :: (Int, Int) -> Int
add_pair (x,y) = x + y

add_fn :: Int -> Int -> Int
add_fn x y = x + y

add_fn2 :: Int -> Int -> Int
add_fn2 = \x y -> x + y

-- add_pair (2,3)
-- 5
-- add_fn 2 3
-- 5
-- add_fn2 2 3
-- 5
-- add_fn3 = curry add_pair
-- add_fn3 2 3
-- 5

curry2 :: ((Int, Int) -> Int) -> (Int -> Int -> Int)
curry2 f = \x y -> f (fst (x,y), snd (x,y))

-- add_fn4 = curry2 add_pair
-- add_fn4 2 3
-- 5

uncurry2 :: (Int -> Int -> Int) -> ((Int, Int) -> Int)
uncurry2 f = \pair -> f (fst pair) (snd pair)

-- add_pair2 = uncurry add_fn
-- add_pair2 (2, 3)
-- 5

-- add_pair3 = uncurry2 add_fn
-- add_pair3 (2, 3)
-- 5

-- Ex 6. unfold p h t x where p - predicate fn, h - head fn, t - tail fn,
-- x - list

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

-- int2bin = unfold (==0)(`mod` 2)(`div` 2)
-- int2bin 5
-- [1,0,1]

type Bit = Int

-- make8 truncates or extends a binary number to make it 8 bits long

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- make8 [1,0,1,1]
-- [1,0,1,1,0,0,0,0]

-- chop8 divides up a long list of bits into 8 bit segments

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- chop8 [1,0,1,1,0,0,0,0, 1,0,1,1,0,1,1,0]
-- [[1,0,1,1,0,0,0,0],[1,0,1,1,0,1,1,0]]

chop8_2 :: [Bit] -> [[Bit]]
chop8_2 = unfold (==[]) (take 8) (drop 8)

-- chop8_2 [1,0,1,1,0,0,0,0, 1,0,1,1,0,1,1,0]
-- [[1,0,1,1,0,0,0,0],[1,0,1,1,0,1,1,0]]

-- map5 :: (a -> b) -> [a] -> [b]
-- map5 f [] = []
-- map5 f (x:xs)  = f x : map5 f xs

map5 :: Eq a => (a -> b) -> [a] -> [b]
map5 f xs = unfold (==[]) (\x -> f (x !! 0)) (tail) xs

-- map5 (^2) [1,2,3,4]

-- :reload
