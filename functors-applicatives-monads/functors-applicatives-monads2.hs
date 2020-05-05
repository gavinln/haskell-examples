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

-- inc 1
-- 3

-- inc (dec 1)
-- 1

-- inc $ dec 1
-- 1

-- (inc . dec) 1
-- 1

-- fmap inc [1]
-- [2]

-- fmap inc (Just 1)
-- Just 2

-- fmap (inc . dec) [1]
-- [1]

-- fmap (inc . dec) (Just 1)
-- Just 1

-- fmap inc (fmap dec [1])
-- [1]

-- fmap inc (fmap dec (Just 1))
-- Just 1

-- fmap inc $ fmap dec [1]
-- [1]

-- id 1
-- 1

-- id [1]
-- [1]

-- fmap id [1]
-- [1]

add :: Num a => a -> a -> a
add x y = x + y

-- add 1 2
-- 3

-- add <$> [1, 2] <*> [1, 3]
-- [2,4,3,5]

-- add <$> (Just 1) <*> (Just 2)
-- Just 3

-- add <$> Nothing <*> (Just 2)
-- Nothing

-- add <$> (Just 1) <*> Nothing
-- Nothing

-- fmap add [1, 2] <*> [1, 3]
-- [2,4,3,5]

-- fmap add (Just 1) <*> (Just 2)
-- Just 3

-- pure add <*> [1, 2] <*> [1, 3]
-- [2,4,3,5]

-- pure add <*> (Just 1) <*> (Just 2)
-- Just 3

-- (Just add) <*> (Just 1) <*> (Just 2)
-- Just 3

-- (Just (add 1)) <*> (Just 2)
-- Just 3

-- [inc, dec] <*> [1, 2]
-- [2,3,0,1]

-- [inc] <*> ([dec] <*> [1, 2])
-- [[1,2]

-- [add] <*> [1, 2] <*> [1, 3]
-- [2,4,3,5]






