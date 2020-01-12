{-
 - Try entering two correct values 3 and 4
 - Try entering incorrect values 3 and abc
 - Try entering no values <press enter> and 4
 -}

import Text.Read (readMaybe)
import Debug.Trace (trace)

compute_area :: Maybe Int -> Maybe Int -> Maybe Int
compute_area Nothing _  = Nothing
compute_area _ Nothing  = Nothing
compute_area (Just x) (Just y) = Just (x * y)

main :: IO ()
main = do
  putStrLn "Enter length"
  length <- getLine
  putStrLn "Enter breadth"
  breadth <- getLine
  let dimensions = fmap readMaybe [length, breadth]
  let area = compute_area (dimensions !! 0) (dimensions !! 1)
  putStrLn ("area = " ++ show area)
  return ()
