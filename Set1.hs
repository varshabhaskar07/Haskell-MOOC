module Set1 where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1
-- Defining simple integer variables with fixed values:
-- "one" is assigned the value 1
one :: Int
one = 1

-- "two" is assigned the value 2
two :: Int
two = 2

------------------------------------------------------------------------------
-- Ex 2
-- Creating a function "double" that takes an Integer as input
-- and returns its value multiplied by 2
double :: Integer -> Integer
double x = x * 2

------------------------------------------------------------------------------
-- Ex 3
-- Creating a function "quadruple" that calculates the value
-- multiplied by 4 by applying the "double" function twice
quadruple :: Integer -> Integer
quadruple x = double (double x)

------------------------------------------------------------------------------
-- Ex 4
-- Creating a function "distance" that calculates the Euclidean distance
-- between two points (x1, y1) and (x2, y2) in a 2D plane.
-- The formula used is sqrt((x2 - x1)^2 + (y2 - y1)^2).
distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

------------------------------------------------------------------------------
-- Ex 5
-- Creating a function "eeny" that returns "eeny" if the input Integer
-- is even, and "meeny" if it is odd
eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

------------------------------------------------------------------------------
-- Ex 6
-- Creating a function "checkPassword" that verifies if the input password
-- matches either "swordfish" or "mellon" and returns a corresponding message
checkPassword :: String -> String
checkPassword password = if password == "swordfish" || password == "mellon"
                         then "You're in."
                         else "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7
-- Creating a function "postagePrice" that calculates the postage cost
-- based on the weight of the package (in grams):
-- - Up to 500 grams: 250 units
-- - Up to 5000 grams: 300 units plus the weight in grams
-- - Over 5000 grams: 6000 units
postagePrice :: Int -> Int
postagePrice w
  | w <= 500 = 250
  | w <= 5000 = 300 + w
  | otherwise = 6000

------------------------------------------------------------------------------
-- Ex 8
-- Creating a function "isZero" that checks if the input Integer is zero.
-- Returns True if the input is zero; otherwise, returns False
isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

------------------------------------------------------------------------------
-- Ex 9
-- Creating a function "sumTo" that calculates the sum of all integers
-- from 1 to the input number n using recursion.
-- For example, sumTo 3 = 1 + 2 + 3 = 6
sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

------------------------------------------------------------------------------
-- Ex 10
-- Creating a function "power" that computes the result of raising
-- a number n to the power of another number k using recursion.
-- For example, power 2 3 = 2^3 = 8
power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k - 1)

------------------------------------------------------------------------------
-- Ex 11
-- Creating a function "ilog3" that calculates the integer logarithm base 3
-- of a number n. It counts how many times n can be divided by 3 before
-- it becomes less than 3.
-- For example, ilog3 27 = 3 (since 27 -> 9 -> 3 -> less than 3)
ilog3 :: Integer -> Integer
ilog3 n
  | n < 3 = 1
  | otherwise = 1 + ilog3 (n `div` 3)
