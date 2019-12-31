-- run using ghci. Type :reload to re-load the file

-- Part 4: Defining functions

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- even' 5

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

-- splitAt' 3 [1, 2, 3, 4]
-- :type splitAt'

recip' :: Fractional a => a -> a
recip' n = 1/n

-- recip' 3

-- Conditional expressions

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

-- Nested conditional expressions

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
              if n == 0 then 0 else 1

-- Guarded equations

abs2 :: Int -> Int
abs2 n | n >= 0    = n
      | otherwise = -n


signum2 :: Int -> Int
signum2 n | n < 0     = -1
         | n == 0    = 0
         | otherwise = 1

-- Pattern matching

not' :: Bool -> Bool
not' False = True
not' True  = False

and2 :: Bool -> Bool -> Bool
and2 True True = True
and2 _    _    = False

and3 :: Bool -> Bool -> Bool
and3 True b  = b
and3 False _ = False

-- using guarded equations

and4 :: Bool -> Bool -> Bool
and4 b c | b == c    = b
         | otherwise = False

-- Tuple patterns
fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b
snd' (_,y) = y

-- List patterns

-- test returns true if the first element in the list is 'a'

test :: [Char] -> Bool
test ['a',_,_] = True
test _         = False

-- cons (construct operator - right associative)

test2 :: [Char] -> Bool
test2 ('a':_) = True
test2  _       = False

head2 :: [a] -> a
head2 (x:_) = x

tail2 :: [a] -> [a]
tail2 (_:xs) = xs

-- Lambda expressions
-- Lambda expressions are nameless functions

-- (\x -> x + x) 2

-- add defined using a lambda funciton

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- const (constant) function returns a constant

const2 :: a -> b -> a
const2 x _ = x

const3 :: a -> (b -> a)
const3 x = \_ -> x

-- Operator sections

-- (#) = \x -> (\y -> x + y)
-- (x #) = \y -> x + y
-- (# y) = \x -> x + y

-- Exercises

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where half = (length xs) `div` 2


third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (a:b:c:_) = c

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _     _     = True

or3 :: Bool -> Bool -> Bool
or3 False b  = b
or3 True  _  = True

-- using guarded equations

or4 :: Bool -> Bool -> Bool
or4 b c | b == c    = b
        | otherwise = True

-- using conditional expressions for and5

and5 :: Bool -> Bool -> Bool
and5 a b = if a then if b then True else False else False

and6 :: Bool -> Bool -> Bool
and6 a b = if a then b else False

-- using lambda expressions

mult2 :: Int -> (Int -> (Int -> Int))
mult2 = \x -> \y -> \z ->  x * y * z

-- :reload
