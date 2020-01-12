{-
Run using the command
runghc 08-main.hs

# Partial application using fmap
-}

import Text.Read (readMaybe)

displayAge maybeAge = 
    case maybeAge of
      Nothing -> putStrLn "You provided an invalid year"
      Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear

main = do
  putStrLn "Please enter your birth year"
  birthYearString <- getLine
  putStrLn "Please enter some year in the future"
  futureYearString <- getLine
  let maybeAge = do
      yearToAge <- fmap yearDiff (readMaybe futureYearString)
      birthYear <- readMaybe birthYearString
      return $ yearToAge birthYear

  displayAge maybeAge

