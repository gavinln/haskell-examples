{-
Run using the command
runghc 12-main.hs

# Making decisions using Monads

Handle birthYear and futureYear in any order
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
      futureYear <- readMaybe futureYearString
      birthYear <- readMaybe birthYearString 
      return $
        if futureYear < birthYear
          then yearDiff birthYear futureYear
          else yearDiff futureYear birthYear

  displayAge maybeAge

