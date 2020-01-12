{-
 - Try entering two correct values 3 and 4
 - Try entering incorrect values 3 and abc
 - Try entering no values <press enter> and 4
 -}

import Text.Read (readMaybe)
import Debug.Trace (trace)

area_rect :: Int -> Int -> Int
area_rect x y = x * y

show_area :: Maybe Int -> IO ()
show_area Nothing = putStrLn ("Invalid length or breadth")
show_area (Just x) = putStrLn ("area = " ++ show x)

main :: IO ()
main = do
  putStrLn "Enter length"
  lengthStr <- getLine
  putStrLn "Enter breadth"
  breadthStr <- getLine
  let area = readMaybe lengthStr >>= \x
              -> readMaybe breadthStr >>= \y
              -> return (area_rect x y)
  show_area area
  return ()
