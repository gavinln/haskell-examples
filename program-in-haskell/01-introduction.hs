-- run using ghci. Type :reload to re-load the file

-- Part 1: Introduction
double x = x + x

-- try double 3
-- try double (double 3)

-- operator .. produces values from 1 to n
-- try [1 .. 5]

sum [1..5]

-- Summing numbers
sumx []    = 0
sumx (x:xs) = x + sumx xs

-- Sorting values
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
               smaller = [a | a <- xs, a <= x]
               larger  = [b | b <- xs, b > x]

seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do x <- acts
                  xs <- seqn acts
                  return (x:xs)


productx [] = 1
productx (x:xs) = x * productx xs

