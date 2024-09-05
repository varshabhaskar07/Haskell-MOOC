module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1

-- Defining a function to count the number of `Nothing` values in a list of `Maybe` values.
countNothings :: [Maybe a] -> Int
countNothings xs = foldr countHelper 0 xs

countHelper :: Maybe a -> Int -> Int
countHelper x acc = maybe (acc + 1) (const acc) x  -- Increment count if `Nothing`, keep count unchanged if `Just`

------------------------------------------------------------------------------
-- Ex 2

-- Defining a function to find the maximum value in a list of integers.
-- The function returns 0 for an empty list.
myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

maxHelper :: Int -> Int -> Int
maxHelper x acc = max x acc  -- Find the maximum of two integers

------------------------------------------------------------------------------
-- Ex 3

-- Defining a function to calculate the sum and length of a list of doubles.
sumAndLength :: [Double] -> (Double, Int)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (0.0, 0)  -- Initial accumulator for sum and length

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (sum, len) = (x + sum, len + 1)  -- Update sum and length

------------------------------------------------------------------------------
-- Ex 4

-- Defining a function to concatenate a list of lists into a single list.
myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart :: [a]
concatStart = []  -- Initial accumulator for concatenation

concatHelper :: [a] -> [a] -> [a]
concatHelper x acc = x ++ acc  -- Append the current list to the accumulator

------------------------------------------------------------------------------
-- Ex 5

-- Defining a function to find the list of largest elements in a list of integers.
largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]  -- Start with the first element if the accumulator is empty
largestHelper x acc@(y:ys)
  | x > y     = [x]  -- New largest element found
  | x == y    = x : acc  -- Element equal to the current largest, add to the list
  | otherwise = acc  -- Current largest is greater than x, keep the accumulator unchanged

------------------------------------------------------------------------------
-- Ex 6

-- Defining a function to get the first element of a list as a `Maybe`.
myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a
headHelper x _ = Just x  -- Return the first element wrapped in `Just`

------------------------------------------------------------------------------
-- Ex 7

-- Defining a function to get the last element of a list as a `Maybe`.
myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x  -- Update accumulator with the last element
lastHelper _ acc     = acc  -- Keep the current accumulator unchanged
