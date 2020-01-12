{-
 - Try entering two correct values 3 and 4
 - Try entering incorrect values 3 and abc
 - Try entering no values <press enter> and 4
 -}
import Text.Read (readMaybe)

compute_area :: Int -> Int -> Int
compute_area length breadth = length * breadth

main :: IO ()
main = do
  putStrLn "Enter length"
  length <- getLine
  putStrLn "Enter breadth"
  breadth <- getLine
  let area = compute_area (read length) (read breadth)
  putStrLn ("area = " ++ show area)
  return ()

