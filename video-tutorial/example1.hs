-- ghc example1.hs
-- ./example1
import Data.List
import System.IO

add :: Num a => a -> a -> a
add x y = x + y

-- add 3 4
-- 7

main = do
  putStrLn "What's your name"
  name <- getLine
  putStrLn ("Hello " ++ name)
