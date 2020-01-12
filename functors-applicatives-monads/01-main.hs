{-
Run using the command
runghc 01-main.hs

# First program

Possible problems
1. No input
2. Non numeric input
-}

main = do
  putStrLn "Please enter your birth year"
  year <- getLine
  putStrLn $ "In 2020, you will be: " ++ show (2020 - read year)
