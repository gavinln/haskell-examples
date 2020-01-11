{-
 Haskell tutorial from this Youtube video

https://www.youtube.com/watch?v=02_H3LjqMr8
-}

-- comment

{-
Multine line comment
 -}

import Data.List
import System.IO

maxInt = maxBound :: Int
-- maxInt
-- 9223372036854775807

minInt = minBound :: Int

-- minInt
-- 9223372036854775807

minDouble = minBound :: Int

-- minDouble
-- -9223372036854775808

boolExample = True

-- boolExample
-- True

charExample  = 'a'

-- charExample
-- 'a'

sumOfNums = sum [1..10]

-- sumOfNums
-- 55

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

-- negNumEx
-- 1

num9 = 9 :: Int

sqrtOf9 = sqrt (fromIntegral num9)

-- sqrtOf9
-- 3.0

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- piVal
-- 3.141592653589793
-- ePow9
-- 8103.083927575384
-- logOf9
-- 2.1972245773362196
-- squared9
-- 81.0
-- truncateVal
-- 9
-- roundVal
-- 10
-- ceilingVal
-- 10
-- floorVal
-- 9

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

-- trueAndFalse
-- False
-- trueOrFalse
-- True
-- notTrue
-- False

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 : []

morePrimes2 = 2 : morePrime

lenPrime = length morePrimes2

-- morePrimes2
-- [2,3,5,7,11,13,17,19,23,29]

-- lenPrime
-- 10

reversePrime = reverse morePrimes2

isListEmpty = null morePrimes2

secondPrime = morePrimes2 !! 1

firstPrime = head morePrimes2

lastPrime = last morePrimes2

-- reversePrime
-- [29,23,19,17,13,11,7,5,3,2]
-- isListEmpty
-- False
-- secondPrime
-- 3
-- firstPrime
-- 2
-- lastPrime
-- 29

primeInit = init morePrimes2

first3Primes = take 3 morePrimes2

removedPrimes = drop 3 morePrimes2

-- primeInit
-- [2,3,5,7,11,13,17,19,23]
-- first3Primes
-- [2,3,5]
-- removedPrimes
-- [2,3,5]

is7InList = 7 `elem` morePrimes2

maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

-- is7InList
-- True
-- maxPrime
-- 29
-- minPrime
-- 2

productList = product [1,2,3]

-- productList
-- 6

zeroToTen = [0..10]

evenList = [2,4..20]

-- zeroToTen
-- [0,1,2,3,4,5,6,7,8,9,10]
-- evenList
-- [2,4,6,8,10,12,14,16,18,20]

letterList = ['A','C'..'Z']

-- letterList
-- "ACEGIKMOQSUWY"

infinPow10 = [10,20..]

-- take 5 infinPow10
-- [10,20,30,40,50]

many2s = take 10 (repeat 2)

-- many2s
-- [2,2,2,2,2,2,2,2,2,2]

many3s = replicate 10 3

-- many3s
-- [3,3,3,3,3,3,3,3,3,3]

cycleList = take 10 (cycle [1,2,3,4,5])

-- cycleList
-- [1,2,3,4,5,1,2,3,4,5]

listTimes2 = [x * 2 | x <- [1..10]]

-- listTimes2
-- [2,4,6,8,10,12,14,16,18,20]

listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 25]

-- listTimes3
-- [3,6,9,12,15,18,21,24]

divBy9And13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

-- divBy9And13
-- [117,234,351,468]

sortedList = sort [9,1,3,2,5,6]

-- sortedList
-- [1,2,3,5,6,9]

sumOfLists = zipWith (+) [1..5] [6..10]

-- sumOfLists
-- [7,9,11,13,15]

listBiggerThan5 = filter (>5) [1..10]

-- listBiggerThan5
-- [6,7,8,9,10]

evensUpto20 = takeWhile (<= 20) [2,4..]

-- evensUpto20
-- [2,4,6,8,10,12,14,16,18,20]

multOfList = foldl (*) 1 [2,3,4,5]

-- multOfList
-- 120

pow3List = [3 ^ n | n <- [1..10]]

-- pow3List
-- [3,9,27,81,243,729,2187,6561,19683,59049]

multTable = [[x * y | y <- [1..4]] | x <- [1..4]]

-- multTable
-- [[1,2,3,4],[2,4,6,8],[3,6,9,12],[4,8,12,16]]

randTuple = (1,"Random Tuple")

-- randTuple
-- (1,"Random Tuple")

bobSmith = ("Bob Smith",52)

bobsName = fst bobSmith

bobsAge = snd bobSmith

-- bobsName
-- "Bob Smith"
-- bobsAge
-- 52

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesNAddress = zip names addresses

-- namesNAddress
-- [("Bob","123 Main"),("Mary","234 North"),("Tom","567 South")]

addMe :: Int -> Int -> Int

addMe x y = x + y

-- addMe 3 4
-- 7

sumMe x y = x + y

-- sumMe 3.4 4.5
-- 7.9

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- addTuples (1,2) (3,4)
-- (4,6)

whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _  = "Nothing important"

-- whatAge 40
-- "Nothing important"

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- factorial 3
-- 6

prodFact n = product [1..n]

-- prodFact 3
-- 6

isOdd :: Int -> Bool
isOdd n | n `mod` 2 == 0 = False
        | otherwise      = True

-- isOdd 3
-- True
-- isOdd 4
-- False

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible batting average"
  | avg <= 0.250 = "Average Playter"
  | avg <= 0.280 = "You are doing pretty good"
  | otherwise = "You're a superstar"
  where avg = hits/atBats

-- batAvgRating 30 100
-- "You're a superstar"

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y 
getListItems (x:xs) = "The first items is " ++ show x ++ " and the rest are " ++ show xs

-- getListItems [1..5]
-- "The first items is 1 and the rest are [2,3,4,5]"

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++  " is " ++ show x

-- getFirstItem "Hello"
-- "The first letter in Hello is 'H'"

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1..5]

-- listTimes4
-- [4,8,12,16,20]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

-- multBy4 [1..5]
-- [4,8,12,16,20]

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

-- areStringsEq "Hello" "Hello"
-- True
-- areStringsEq "Hello" "Hello You"
-- False

doMul :: (Int -> Int) -> Int
doMul func = func 3

-- doMul times4 3
-- 12

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3

-- adds3 4
-- 7

-- map adds3 [1..5]
-- [4,5,6,7,8]

dbl1To10 = map (\x -> x * 2) [1..10]

-- dbl1To10
-- [2,4,6,8,10,12,14,16,18,20]

doubleEvenNumber y = (if y `mod` 2 /= 0 then y else y * 2)

-- doubleEvenNumber 100
-- 200
-- doubleEvenNumber 101
-- 101

getClass :: Int -> String
getClass n = case n of
  5 -> "Go to Kindergarten"
  6 -> "Go to elementary school"
  _ -> "Go away"

-- getClass 5
-- "Go to Kindergarten"
-- getClass 15
-- "Go away"

data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

-- print(barryBonds Outfield)
-- True

data Customer = Customer String String Double deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

-- getBalance tomSmith
-- 20.5

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String

shoot Paper Rock = "Paper beats Rock"
shoot Paper Scissors = "Paper loses to Scissors"
shoot Rock Paper =  "Rock beats Paper"
shoot Rock Scissors = "Rock beats Scissors"
shoot Scissors Paper = "Scissors beats Paper"
shoot Scissors Rock = "Scissors loses to Rock"
shoot _ _ = "Error"

-- shoot Paper Rock
-- "Paper beats Rock"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- area (Circle 10 20 3)
-- 28.274334

-- area $ Rectangle 10 10 100 100
-- 8100.0

sumValue = putStrLn (show (1 + 2))

sumValue2 = putStrLn . show $ 1 + 2

-- sumValue
-- 3
-- sumValue2
-- 3

data Employee = Employee {
    name :: String,
    position :: String,
    idNum :: Int
  } deriving (Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 3}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 4}

isSamPam = samSmith == pamMarx

-- isSamPam
-- False

-- show samSmith
-- "Employee {name = \"Sam Smith\", position = \"Manager\", idNum = 3}"

data ShirtSize = S | M | L

instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvail = S `elem` [S, M, L]

theSize = show S

-- smallAvail
-- True

-- theSize
-- "Small"

class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = True

-- areEqual M M
-- True

sayHello = do
  putStrLn "What's your name"
  name <- getLine
  putStrLn $ "Hello " ++ name

-- sayHello

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random line of text")
  hClose theFile

readFromFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile
  putStr contents
  hClose theFile

-- writeToFile

-- readFromFile
-- Random line of text

fib = 1 : 1 : [a + b | (a,b) <- zip fib (tail fib)]

-- take 5 fib
-- [1,1,2,3,5,8,13,21,34,55]

fib20 = fib !! 20

-- fib20
-- 10946
