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

