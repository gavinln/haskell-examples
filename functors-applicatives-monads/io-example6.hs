{-
 - Try entering two correct values 3 and 4
 - Try entering incorrect values "x" and 4
 - Try entering no values <press enter> and 4
 -}

import Text.Read (readMaybe)

compute_area :: Int -> Int -> Int
compute_area length breadth = length * breadth

show_area :: Maybe Int -> IO ()
show_area Nothing = putStrLn ("Invalid length or breadth")
show_area (Just x) = putStrLn ("area = " ++ show x)

main :: IO ()
main = do
  putStrLn "Enter length"
  lengthStr <- getLine
  putStrLn "Enter breadth"
  breadthStr <- getLine
  let area = do
              length <- readMaybe lengthStr
              breadth <- readMaybe breadthStr
              return (compute_area length breadth)
  show_area area
  return ()
