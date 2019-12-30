-- run using ghci. Type :reload to re-load the file

-- Part 3: Types and classes

False :: Bool

True :: Bool

not 1
-- has an error

not False
-- True

1 `div` 0
-- raises a divide by zero exception

:type not
-- not :: Bool -> Bool

:type False
-- False :: Bool

:type not False
-- not False :: Bool

{-
 - Basic types
 -}

-- Char type
:type 'A'
:type '_'
:type '\n'

-- Int types
:type 300
:type -20
:type 0
:type 2^60

-- Integer types
:type 2 ^ 128 :: Integer

-- Float types
:type -12.34
:type 1.0
:type 3.1415

-- Double types
:type 1.4142 :: Double
:type sqrt 2 :: Double

-- List types
:type [False,True,False]
-- [Bool]

:type ['a','b','c','d']
-- [Char]

:type ["one","two","three"] :: [String]
-- [String]

length []
-- 0

length [[]]
-- 1

:type [['a','b'],['c']]
-- [[Char]]

-- Tuples
-- finite sequence of components of different types

:type (False,True)
-- (Bool, Bool)

:type (False,'a',True)
-- (Bool, Char, Bool)

-- empty tuple with arity (length) zero

:type ()

-- not a tuple

:type (3)

:type ('a',(False,'b'))
-- (Char, (Bool, Char))

:type (['a'],[False,True])
-- ([Char], [Bool])

:type [('a',False),('b',True)]
-- [(Char, Bool)]
