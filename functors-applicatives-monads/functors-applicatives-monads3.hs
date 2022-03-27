{- From the Haskell Typeclassopedia
 - https://wiki.haskell.org/Typeclassopedia
 -
 - :botright Tnew
 - win-linux.bat
 - cd walkthrough
 - nix-shell shell.nix
 - cd ../functors-applicatives-monads
 - ghci functors-applicatives-monads2.hs
 - run using ghci. Type :reload to re-load the file
 - Type :t inc to get type definion
 -}

{-# LANGUAGE FlexibleContexts #-}
import Text.Read (readMaybe)

-- :set -XFlexibleContexts

main = putStrLn "In main"

inc x = x + 1

double x = 2 * x

-- inc 3
-- double 2
-- inc (double 2)

incDouble x = inc (double x)

incDouble2 x = inc . double x

incDouble3 = inc . double

-- incDouble 2
-- incDouble2 2
-- incDouble3 2

-- fmap inc [2]
-- fmap inc [2, 3]

a = [2]
b = [2, 3]

-- fmap inc a
-- fmap inc b
-- fmap double a
-- fmap double b
-- fmap incDouble a
-- fmap incDouble b

c = Just 2
d = Nothing

-- fmap inc c
-- fmap inc d

incList x = [inc x]

incMaybe x = Just (inc x)

incWrap x = return (inc x)

-- [2] >>= incList
-- Just 2 >>= incMaybe
-- Just 2 >>= incMaybe

-- [2] >>= incWrap
-- Just 2 >>= incWrap
-- Nothing >>= incWrap

-- [2] >>= \x -> return (inc x)
-- Just 2 >>= \x -> return (inc x)
-- [2] >>= \x -> return (inc x) >>= \x -> return (double x)

-- IO

getPut = getLine >>= putStrLn

addSuffix x = (x ++ "_suffix") :: String

-- getLine >>= \x -> print(addSuffix x)
-- getLine >>= print . addSuffix

readMaybeInt x = readMaybe x :: Maybe Int

-- :t getLine
-- getLine >>= \x -> print(readMaybeInt x)
-- getLine >>= print . readMaybeInt

displayResult x = case x of
        Just a -> print a
        Nothing -> print "invalid"

getLineInt :: IO Int
getLineInt = do
  putStrLn "Please enter a number"
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn "Invalid number entered" >> getLineInt

-- getLineInt >>= incWrap

