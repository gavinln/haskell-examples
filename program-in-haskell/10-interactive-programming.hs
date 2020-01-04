-- run using ghci. Type :reload to re-load the file

-- Part 10: Interactive programming

-- print("hello")

-- 10.3 Basic actions

-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a

-- getChar
-- putChar '3'

-- 10.4 Sequencing

import System.IO

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x,y)

-- act

-- 10.5 Derived primitives

getLine2 :: IO String
getLine2 = do
  x <- getChar
  if x == '\n' then
    return []
  else do
    xs <- getLine2
    return (x:xs)

-- getLine2

putStr2 :: String -> IO ()
putStr2 [] = return ()
putStr2 (x:xs) = do
  putChar x
  putStr xs

-- putStr "abc"

putStrLn2 :: String -> IO ()
putStrLn2 xs = do
  putStr2 xs
  putChar '\n'

-- putStrLn2 "abc"
-- abc

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters"

-- strlen

-- Ex. 10.6 Hangman

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n' then
    do
      putChar x
      return []
  else
    do
      putChar '-'
      xs <- sgetLine
      return (x:xs)

-- sgetLine

-- match word guess

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- match "bee" "cab"
-- "b--"

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word then
    putStrLn "You got it!!"
  else
    do
      putStrLn (match word guess)
      play word

hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

-- :reload
