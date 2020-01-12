{-
Run using the command
runghc 10-main.hs

# Partial application using applicatives
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
  let maybeAge = fmap yearDiff (readMaybe futureYearString)
                    <*> readMaybe birthYearString

  displayAge maybeAge

