{-
 - Monads as computation
 - https://wiki.haskell.org/Monads_as_computation
 -
 - nix-shell
 - ghci monads-computation.hs
 - :q  -- to quit
 -}

{-
  - return :: (Monad m) => a -> m a
  - (>>) :: (Monad m) => m a -> m b -> m b
  - (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-}

data Maybe2 a = Nothing2 | Just2 a deriving Show

instance Functor Maybe2 where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Nothing2 = Nothing2
  fmap f (Just2 x) = Just2 (f x)

instance Applicative Maybe2 where
  -- pure :: a -> Maybe a
  pure x = Just2 x

  -- (<*>) :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
  (Just2 g) <*> mx = fmap g mx

instance Monad Maybe2 where
  -- (>>=) :: m a -> (a -> m b) -> m b
  Nothing2 >>= _ = Nothing2
  (Just2 x) >>= f = f x


-- fmap (+1) (Just2 3)
-- Just2 4
-- pure (+1) <*> Just2 3
-- Just2 4
-- pure (+) <*> Just2 3 <*> Just2 2
-- Just2 5
-- (+) <$> Just2 3 <*> Just2 2
-- Just2 5
-- Just2 3 >>= \x -> Just2 (x + 1)
-- Just2 4
-- Just2 3 >>= \x -> (Just2 2) >>= \y -> Just2 (x + y)
-- Just2 5
-- do { x <- Just2 3; y <- Just2 2; return (x + y);}
-- Just2 5

main1 :: IO ()
main1 = getLine >>= putStrLn

main2 :: IO ()
main2 = putStrLn "Enter a line of text:"
        >> getLine >>= \x -> putStrLn (reverse x)

{-
do { x } = x

do { x ; <stmts> }
  = x >> do { <stmts> }

do { v <- x ; <stmts> }
  = x >>= \v -> do { <stmts> }

do { let <decls> ; <stmts> }
  = let <decls> in do { <stmts> }
-}

sequence2 :: (Monad m) => [m a] -> m [a]
sequence2 []     = return []
sequence2 (x:xs) = do
                      v <- x
                      vs <- sequence2 xs
                      return (v:vs)

-- sequence2 [Just 2, Just 3]
-- Just [2,3]
-- sequence2 [Just 2, Nothing, Just 3]
-- Nothing

sequence3 :: (Monad m) => [m a] -> m [a]
sequence3 []     = return []
sequence3 (x:xs) = x >>= \v -> sequence3 xs >>= \vs -> return (v:vs)

-- sequence3 [Just 2, Just 3]
-- Just [2,3]
-- sequence3 [Just 2, Nothing, Just 3]
-- Nothing

main3 = sequence [getLine, getLine] >>= print

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM xs f = sequence (map f xs)

main4 = forM [1..3] $ \x -> do
          putStr "Looping: "
          print x

-- main4
-- Looping: 1
-- Looping: 2
-- Looping: 3
-- [(),(),()]
