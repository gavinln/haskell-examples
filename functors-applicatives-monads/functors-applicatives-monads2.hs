{- From the Haskell Typeclassopedia
 - https://wiki.haskell.org/Typeclassopedia
 -
 - :botright Tnew
 - win-linux.bat
 - cd walkthrough
 - nix-shell shell.nix
 - cd ../functors-applicatives-monads
 - ghci functors-applicatives-monads2.hs
-- run using ghci. Type :reload to re-load the file
-- Type :t inc to get type definion
 -}

inc :: Num a => a -> a
inc x = x + 1

dec :: Num a => a -> a
dec x = x - 1

inc2 :: Num a => Maybe a -> Maybe a
inc2 Nothing = Nothing
inc2 (Just x) = Just (x + 1)

dec2 :: Num a => Maybe a -> Maybe a
dec2 Nothing = Nothing
dec2 (Just x) = Just (x - 1)

add :: Num a => a -> a -> a
add x y = x + y

---- Basic syntax

-- inc 3
-- 4

-- add 1 2
-- 3

-- inc [3]  -- will fail

---- Functor
-- apply regular functions to wrapped values

-- fmap inc [3]  -- apply inc to a list
-- [4]

-- fmap inc (Just 3)  -- apply inc to a Maybe
-- Just 4

-- fmap inc Nothing  -- apply inc to a Maybe
-- Nothing

---- Applicative
-- apply wrapped functions to wrapped values

-- [inc] <*> [3]  -- inc wrapped in a list applied to list
-- [4]

-- Just inc <*> Just 3  -- inc wrapped in a Maybe applied to a Maybe
-- Nothing <*> Just 3

-- pure inc <*> [3]  -- wrapping using pure works with both lists and Maybes
-- pure inc <*> Just 3

-- [inc, dec] <*> [3]  -- wrapping allows multiple functions to be applied
-- [4,2]

-- [inc, dec] <*> [3, 6]  -- multiple wrapped functions & multiple wrapped values
-- [4,7,2,5]

-- area x y = x * y  -- area of a rectangle (width * height)

-- perimeter x y = 2 * x + 2 * y  -- perimeter of a rectangle

-- area 3 4
-- 12

-- perimeter 3 4
-- 14

-- [area, perimeter] <*> [3] <*> [4]  -- calculate area and perimeter
-- [12,14]

-- pure area <*> Just 3 <*> Just 4  -- using Maybe values
-- Just 12

-- pure area <*> Nothing <*> Just 4  -- using Nothing works
-- Nothing

-- pure area <*> Just 3 <*> Nothing
-- Nothing

-- pure area <*> Nothing <*> Nothing
-- Nothing

---- Using $ notation

-- inc 3
-- 4

-- inc $ 3  -- $ not very useful for one parameter function by itself
-- 4

-- inc (dec 3)
-- 3

-- inc $ dec 3  -- alternative to using parenthesis ()
-- 3

-- inc <$> [1, 2]  -- wrap and apply inc function to wrapped values
-- [2,3]

-- pure inc <*> [1,2]  -- more tedious
-- [2,3]

-- fmap inc [1, 2]
-- [2,3]

-- add 2 3
-- 5

-- add <$> [2] <*> [3]  -- wrap function add and apply to two wrapped parameters
-- [5]

-- pure add <*> [2] <*> [3]  -- more tedious
-- [5]

-- fmap add [2] <*> [3]
-- [5]

-- add <$> Just 2 <*> Just 3
-- Just 5

-- add <$> Just 2 <*> Nothing
-- Nothing

-- add <$> [1, 2] <*> [1, 3]
-- [2,4,3,5]

-- add <$> (Just 1) <*> (Just 2)
-- Just 3

-- add <$> Nothing <*> (Just 2)
-- Nothing

-- add <$> (Just 1) <*> Nothing
-- Nothing

-- add <$> [1, 2] <*> [3, 6]
-- [2,4,3,5]

-- [inc, dec] <*> [1]
-- [2,0]

-- [inc, dec] <*> [1, 4]
-- [2,5,0,3]

-- Just add <*> Just 1 <*> Just 2
-- Just 3

-- Just (add 1) <*> Just 2
-- Just 3

-- inc <$> dec <$> [1, 2]  -- multiple functions to the same wrapped inputs
-- [1,2]

-- [inc] <*> ([dec] <*> [1, 2])
-- [1,2]

-- inc . dec <$> [1, 2]  -- compose functions
-- [1,2]




