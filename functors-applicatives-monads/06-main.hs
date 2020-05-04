{-
Run using the command
runghc 06-main.hs

# Sequencing operations
-}

import Text.Read (readMaybe)

displayAge maybeAge = 
    case maybeAge of
      Nothing -> putStrLn "You provided an invalid year"
      Just age -> putStrLn $ "In tha year, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
  putStrLn "Please enter your birth year"
  birthYearString <- getLine
  putStrLn "Please enter some year in the future"
  futureYearString <- getLine
  let maybeAge =
        case readMaybe birthYearString of
          Nothing -> Nothing
          Just birthYear -> 
            case readMaybe futureYearString of
              Nothing -> Nothing
              Just futureYear -> Just (futureYear - birthYear)

  displayAge maybeAge

