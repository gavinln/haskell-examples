{-
Run using the command
runghc 02-main.hs

# Check input
-}

import Text.Read (readMaybe)

main = do
  putStrLn "Please enter your birth year"
  yearString <- getLine
  case readMaybe yearString of
    Nothing -> putStrLn "You provided an invalid year"
    Just year -> putStrLn $ "In 2020, you will be: " ++ show (2020 - year)
