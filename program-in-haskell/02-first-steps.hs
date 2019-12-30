-- run using ghci. Type :reload to re-load the file

-- Part 2: First steps

-- Standard prelude function

-- head - first element of a non-empty list
head [1,2,3,4,5]
-- 1

-- tail - remove first element of a non-empty list
tail [1,2,3,4,5]
-- [2,3,4,5]

-- select the nth element of a list, counting from zero
[1,2,3,4,5] !! 2
-- 3

-- select the first n elments of a list
take 3 [1,2,3,4,5]
-- [1,2,3]

-- drop the first n elements from a list
drop 3 [1,2,3,4,5]
-- [4,5]

-- lenght of a list
length [1,2,3,4,5]
-- 5

-- sum of a list of numbers
sum [1,2,3,4,5]
-- 15

-- product of a list of numbers
product [1,2,3,4,5]
-- 120

-- append two lists
[1,2,3] ++ [4,5]
-- [1,2,3,4,5]

-- reverse a list
reverse [1,2,3,4,5]
[5,4,3,2,1]

-- mathematics vs haskell function application
-- f(x)      | f x
-- f(x,y)    | f x y
-- f(g(x))   | f (g x)
-- f(x,g(y)) | f x (g y)
-- f(x)g(y)  | f x * g y

double x = x + x

quadruple x = double (double x)

quadruple 10
-- 40

take (double 2) [1,2,3,4,5]
-- [1,2,3,4]

factorial n = product [1..n]

average ns = sum ns `div` length ns

factorial 10

average [1,2,3,4,5]

{-
 - Nested comment
 -}

-- Exercises
2 ^ 3 * 4
(2 ^ 3) * 4

2 * 3 + 4 * 5
(2 * 3) + (4 * 5)

2 + 3 * 4 ^ 5
2 + (3 * (4 ^ 5))

n = a `div` (length xs) where { a = 10; xs = [1,2,3,4,5] }

c = a + b where { a = 3; b = 2 }

-- define last function in terms of other functions
lastx xs = xs !! (length xs - 1)

lastx [2,3,4]

-- define init using other functions

init [1,2,3,4,5]
-- [1,2,3,4,5]

initx xs = take ((length xs) - 1) xs

initx [1,2,3,4,5]

