{-
 - Monads IO
 - https://wiki.haskell.org/Monads_as_containers
 -}

-- putStrLn "Hello, World!" :: IO ()

{-
(>>):: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k
-}

main1 = do
        putStrLn "Hello, what is your name?"
        name <- getLine
        putStrLn ("Hello " ++ name ++ "!")

main2 = putStrLn "Hello, what is your name?" >>
        getLine >>= \name ->
        putStrLn ("Hello " ++ name ++ "!")

main3 = putStrLn "Hello, what is your name?" >>= \_ ->
        getLine >>= \name ->
        putStrLn ("Hello " ++ name ++ "!")

-- main1
-- main2
-- main3

-- Reader monad

-- State monad
