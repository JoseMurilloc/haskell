module Script () where
import Data.List
import Data.Map (Map)
 

{-  Module
  Comments to used in multiples lines
  Jose Murillo 
 -}
sumDigits :: Int -> Int
sumDigits n
	| n == 0 = 0
	| otherwise = mod n 10 + sumDigits (div n 10)

biggerDigit :: Integer -> Integer
biggerDigit n
	| n == 0 = 0
	| otherwise = max (mod n 10) (biggerDigit (div n 10))

-- TYPES
age :: Int
age = 24

name :: [Char]
name = ['M', 'u', 'r', 'i', 'l', 'l', 'o']

list = []

tuples = ()

fullName :: String
fullName = "Murillo"

trueAndFalse :: Bool
trueAndFalse = True && False

trueOrFalse :: Bool
trueOrFalse = True || False

notTrue :: Bool
notTrue = not True

numFive :: Float
numFive = 5.0

-- FUNCTIONS
numberPair :: Int -> Bool
numberPair x = (mod x 2) == 0

numberOdd :: Int -> Bool
numberOdd x = (mod x 2) /= 0

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

maxi :: Int -> Int -> Int
maxi n m | n > m = n
         | n == m = n
         | otherwise = m

xor :: Bool -> Bool -> Bool
xor x y  = (x || y) && not (x && y)

-- PATTER MATCH
eXor :: Bool -> Bool -> Bool
eXor True x = not x
eXor False x = x

-- LOCAL DEFINITION
sumSquares :: Int -> Int -> Int
sumSquares x y = sqX + sqY
  where sqX = x * x
        sqY = y * y

otherSumSquares :: Int -> Int -> Int
otherSumSquares x y = sq x + sq y
  where sq z = z * z

anotherSumSquares :: Int -> Int -> Int
anotherSumSquares x y = let sqX = x * x
                            sqY = y * y
                        in sqX + sqY

--  Loops (?)

plusOne :: Int -> Int
plusOne x = x + 1

listNumbers = [1,2,3,4] :: [Int]

map plusOne listNumbers

isEven n = mod n 2 == 0

filter isEven listNumbers