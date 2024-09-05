{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1 
-- Defining a function to build a list that starts with 'start', repeats 'count' times, and ends with 'end'.
-- The list starts with 'start', has 'count' occurrences of 'start', and ends with 'end'.
buildList :: Int -> Int -> Int -> [Int]
buildList start 0 end = [end]  -- When count is 0, the list consists only of the 'end' element
buildList start count end = start : buildList start (count - 1) end  -- Recursively building the list

------------------------------------------------------------------------------
-- Ex 2 
-- Defining a function to calculate cumulative sums from 1 up to 'i'.
-- The result is a list of cumulative sums for each integer from 1 to 'i'.
sums :: Int -> [Int]
sums i = sumsHelper i 1 0  -- Initial call to the helper function with starting index 1 and sum 0

sumsHelper :: Int -> Int -> Int -> [Int]
sumsHelper i n sum
  | n > i     = []  -- Base case: if n is greater than i, return an empty list
  | otherwise = newSum : sumsHelper i (n + 1) newSum  -- Recursively calculating cumulative sums
  where newSum = sum + n  -- Calculating the new cumulative sum

------------------------------------------------------------------------------
-- Ex 3 
-- Defining a function to find the last element of a list, with a default value if the list is empty.
-- The function returns the last element if available; otherwise, it returns the provided default value.
mylast :: a -> [a] -> a
mylast def [] = def  -- Return the default value if the list is empty
mylast def [x] = x  -- Return the single element if the list contains only one element
mylast def (_:xs) = mylast def xs  -- Recursively finding the last element

------------------------------------------------------------------------------
-- Ex 4
-- Defining a function to access an element in a list by index, with a default value if the index is out of bounds.
-- The function returns the element at the specified index or the default value if the index is invalid.
indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def  -- Return the default value if the list is empty
indexDefault (x:xs) 0 _ = x  -- Return the first element if the index is 0
indexDefault (x:xs) i def
  | i < 0     = def  -- Return the default value if the index is negative
  | otherwise = indexDefault xs (i - 1) def  -- Recursively accessing the element at the specified index

------------------------------------------------------------------------------
-- Ex 5 
-- Defining a function to check if a list of integers is sorted in non-decreasing order.
-- The function returns True if the list is sorted; otherwise, it returns False.
sorted :: [Int] -> Bool
sorted [] = True  -- An empty list is considered sorted
sorted [x] = True  -- A list with a single element is considered sorted
sorted (x:y:xs) = x <= y && sorted (y:xs)  -- Recursively checking if each pair of elements is in non-decreasing order

------------------------------------------------------------------------------
-- Ex 6 
-- Defining a function to generate a list of cumulative sums of integers from a given list.
-- The result is a list where each element is the sum of the elements from the beginning of the list up to that position.
sumsOf :: [Int] -> [Int]
sumsOf = sumsOfHelper 0  -- Initial call to the helper function with starting accumulator 0

sumsOfHelper :: Int -> [Int] -> [Int]
sumsOfHelper _ [] = []  -- Return an empty list if the input list is empty
sumsOfHelper acc (x:xs) = newSum : sumsOfHelper newSum xs  -- Recursively calculating cumulative sums
  where newSum = acc + x  -- Calculating the new cumulative sum

------------------------------------------------------------------------------
-- Ex 7 
-- Defining a function to merge two sorted lists of integers into a single sorted list.
-- The function assumes both input lists are already sorted.
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys  -- If the first list is empty, return the second list
merge xs [] = xs  -- If the second list is empty, return the first list
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)  -- Adding x to the result if it is less than or equal to y
  | otherwise = y : merge (x:xs) ys  -- Adding y to the result if it is less than x

------------------------------------------------------------------------------
-- Ex 8 
-- Defining a function to find the maximum element in a list based on a comparison function.
-- The comparison function determines which element is considered larger.
mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial  -- Return the initial value if the list is empty
mymaximum bigger initial (x:xs)
  | initial `bigger` x = mymaximum bigger initial xs  -- Recursively finding the maximum element based on the comparison function
  | otherwise          = mymaximum bigger x xs

------------------------------------------------------------------------------
-- Ex 9 
-- Defining a function to map a function over two lists to produce a list of results.
-- The function applies the given function to corresponding elements from both lists.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []  -- Return an empty list if the first list is empty
map2 _ _ [] = []  -- Return an empty list if the second list is empty
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys  -- Applying the function to the heads of both lists and recursively processing the rest

------------------------------------------------------------------------------
-- Ex 10
-- Defining a function to map a function that returns 'Maybe' over a list and filter out 'Nothing' results.
-- The function applies the given function to each element in the list and returns only the 'Just' values.
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []  -- Return an empty list if the input list is empty
maybeMap f (x:xs) = case f x of
  Nothing -> maybeMap f xs  -- Skip the element if the function returns Nothing
  Just y  -> y : maybeMap f xs  -- Include the element if the function returns Just y
