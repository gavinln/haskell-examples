{-
 - Introduction to IO in Haskell
 - https://wiki.haskell.org/Introduction_to_IO
 -}


{-
getLine :: IO String
putStrLn :: String -> IO () -- note that the result value is an empty tuple.
 -}

-- getLine
-- putStrLn "Hello world"
-- Hello world

main :: IO ()
main = putStrLn "Hello, world!"

{-
 - (>>) :: IO a -> IO b -> IO b
 - if x and y are IO actions (x >> y) action means perform x, drop the result,
 - perform y and return the result
 -}

main2 = putStrLn "Hello, " >> putStrLn "world!"

{-
 - (>>=) :: IO a -> (a -> IO b) -> IO b
 - chains actions where the output of the first action is used as the input of
 - the second action (x >>= f) action means perform x then send the results to
 - function f
 -}

main3 = putStrLn "Hello, what is your name?"
        >> getLine
        >>= \name -> putStrLn ("Hello, " ++ name ++ "!")

{-
 - To turn a value into an action, particularly at the end of a series of
 - actions use the function:
 - return :: a -> IO a
 -}

main4 = do
        putStrLn "Hello, what is your name?"
        name <- getLine
        putStrLn ("Helo, " ++ name ++ "!")

{-
 - The do notation is converted into the bind notation above using
 - a combination of ">>" and ">>=" and lambdas. Expressions of the form "v <- x" will
 - cause action x to be run and the result bound to the variable v. To make
 - a variable binding in a do block use a "let a = b" line.
 -}
