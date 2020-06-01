{-
 - Try entering two correct values 3 and 4
 - Try entering incorrect values "x" and "y"
 - Call either compute_area or display_area
 - Try entering no values <press enter> and 4
 -}
import Text.Read (readMaybe)

compute_area :: Int -> Int -> Int
compute_area length breadth = length * breadth

display_area :: String -> String -> String
display_area length breadth = length ++ " * " ++ breadth

main :: IO ()
main = do
  putStrLn "Enter length"
  length <- getLine
  putStrLn "Enter breadth"
  breadth <- getLine
  let area = compute_area (read length) (read breadth)
  -- let area = display_area (read length) (read breadth)
  putStrLn ("area = " ++ show area)
  return ()

