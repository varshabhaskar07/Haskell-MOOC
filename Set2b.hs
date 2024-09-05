module Set2b where

import Mooc.Todo
import Data.List

------------------------------------------------------------------------------
-- Ex 1
-- Defining a function to calculate binomial coefficients using recursion.
-- The binomial coefficient C(n, k) represents the number of ways to choose
-- k items from n items without regard to order.
-- It is calculated as C(n-1, k) + C(n-1, k-1).
binomial :: Integer -> Integer -> Integer
binomial n k
  | k == 0        = 1  -- C(n, 0) is always 1
  | n == 0        = 0  -- C(0, k) is 0 for k > 0
  | k > n         = 0  -- C(n, k) is 0 if k > n
  | otherwise     = binomial (n-1) k + binomial (n-1) (k-1)  -- Recursive calculation

------------------------------------------------------------------------------
-- Ex 2
-- Defining a function to calculate the factorial of odd numbers only.
-- The factorial of an odd number n is calculated by multiplying n with the
-- factorial of the next smaller odd number.
oddFactorial :: Integer -> Integer
oddFactorial n
  | n <= 0        = 1  -- Returning 1 for non-positive n
  | n `mod` 2 == 0 = oddFactorial (n-1)  -- Skipping even numbers
  | otherwise     = n * oddFactorial (n-2)  -- Multiplying n with the factorial of the previous odd number

------------------------------------------------------------------------------
-- Ex 3
-- Defining a function to calculate the greatest common divisor (GCD) using
-- the Euclidean algorithm. The GCD of two numbers a and b is the largest number
-- that divides both of them without leaving a remainder.
myGcd :: Integer -> Integer -> Integer
myGcd a b
  | b == 0    = a  -- Returning a when b is 0
  | otherwise = myGcd b (a `mod` b)  -- Recursively calculating GCD

------------------------------------------------------------------------------
-- Ex 4
-- Defining a function to pad a string on the left with spaces until it reaches
-- a specified length. If the string is already longer than or equal to the desired length,
-- it is returned unchanged.
leftpad :: String -> Int -> String
leftpad s n
  | length s >= n = s  -- Returning the string if it is already long enough
  | otherwise     = replicate (n - length s) ' ' ++ s  -- Prepending spaces to the string

------------------------------------------------------------------------------
-- Ex 5
-- Defining a function to create a countdown string starting from a given number.
-- The countdown string starts with "Ready!", followed by the countdown from n to 1,
-- and ends with "Liftoff!".
countdown :: Integer -> String
countdown n = "Ready! " ++ go n ++ "Liftoff!"  -- Starting the countdown
  where
    go 0 = ""  -- Stopping the countdown at 0
    go x = show x ++ "... " ++ go (x-1)  -- Recursively creating the countdown string

------------------------------------------------------------------------------
-- Ex 6
-- Defining a function to find the smallest divisor of a given number greater than 1.
-- It starts searching from 2 and continues until it finds a number that divides n evenly.
smallestDivisor :: Integer -> Integer
smallestDivisor n = findDivisor n 2  -- Starting the search from 2
  where
    findDivisor n k
        | n `mod` k == 0 = k  -- Returning k if it divides n evenly
        | otherwise = findDivisor n (k + 1)  -- Continuing the search with the next number

------------------------------------------------------------------------------
-- Ex 7
-- Defining a function to check if a number is prime.
-- A prime number is greater than 1 and has no divisors other than 1 and itself.
isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False  -- Returning False for numbers less than or equal to 1
    | otherwise = smallestDivisor n == n  -- Checking if the smallest divisor is the number itself

------------------------------------------------------------------------------
-- Ex 8
-- Defining a function to find the largest prime number less than or equal to a given number.
-- If the number is less than 2, an error is raised as there are no primes below 2.
-- The function recursively finds the largest prime number less than or equal to n.
biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n
    | n < 2 = error "no primes below 2"  -- Throwing an error if n is less than 2
    | isPrime n = n  -- Returning n if it is prime
    | otherwise = biggestPrimeAtMost (n - 1)  -- Recursively finding the largest prime number less than n
