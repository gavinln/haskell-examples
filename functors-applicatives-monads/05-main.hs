{-
Run using the command
runghc 05-main.hs

# Do notation
-}

import Text.Read (readMaybe)

displayAge maybeAge = 
    case maybeAge of
      Nothing -> putStrLn "You provided an invalid year"
      Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
  putStrLn "Please enter your birth year"
  yearString <- getLine
  let maybeAge = do
      yearInteger <- readMaybe yearString
      return $ yearToAge yearInteger

  displayAge maybeAge

