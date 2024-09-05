module Set3a where

import Mooc.Todo
import Data.Char
import Data.Either
import Data.List

------------------------------------------------------------------------------
-- Ex 1
-- Defining a function to determine the maximum of two elements based on a measure function.
-- The function returns the element for which the measure function yields the greater value.
maxBy :: (a -> Int) -> a -> a -> a
maxBy measure a b 
  | measure a >= measure b = a  -- Returning a if its measure is greater than or equal to b's measure
  | otherwise = b              -- Returning b otherwise

------------------------------------------------------------------------------
-- Ex 2
-- Defining a function to apply a given function to a Maybe value.
-- If the input is Nothing, the result is also Nothing.
-- If the input is Just x, the function is applied to x, and the result is wrapped in Just.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing      -- Returning Nothing if the input is Nothing
mapMaybe f (Just x) = Just (f x)  -- Applying function f to x and wrapping the result in Just

------------------------------------------------------------------------------
-- Ex 3
-- Defining a function to apply a binary function to two Maybe values.
-- If both inputs are Just values, the function is applied and the result is wrapped in Just.
-- If either input is Nothing, the result is Nothing.
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 f (Just x) (Just y) = Just (f x y)  -- Applying function f if both inputs are Just values
mapMaybe2 f _ _ = Nothing                     -- Returning Nothing if either input is Nothing

------------------------------------------------------------------------------
-- Ex 4 
-- Defining a function to extract the first half of a string.
-- If the string length is odd, the extra character is included in the first half.
firstHalf :: String -> String
firstHalf s = take ((length s + 1) `div` 2) s  -- Taking the first half of the string

-- Defining a function to check if a string is a palindrome.
-- A string is considered a palindrome if it reads the same forwards and backwards.
palindrome :: String -> Bool
palindrome s = s == reverse s                 -- Checking if the string is equal to its reverse

------------------------------------------------------------------------------
-- Ex 5
-- Defining a function to capitalize the first letter of each word in a string.
-- Words are separated by spaces.
capitalize :: String -> String
capitalize = unwords . map capitalizeFirst . words  -- Capitalizing the first letter of each word

-- Defining a helper function to capitalize the first letter of a word.
-- The rest of the word remains unchanged.
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs            -- Capitalizing the first letter of the word

------------------------------------------------------------------------------
-- Ex 6
-- Defining a function to generate a list of powers of k that do not exceed a maximum value.
-- The function returns powers of k (k^i) for i starting from 0 up to the maximum value.
powers :: Int -> Int -> [Int]
powers k max = takeWhile (<= max) [k^i | i <- [0..]]  -- Generating powers of k that are less than or equal to max

------------------------------------------------------------------------------
-- Ex 7 
-- Defining a function to iteratively update a value while a condition holds true.
-- The function keeps applying the update function until the condition is no longer met.
while :: (a -> Bool) -> (a -> a) -> a -> a
while check update value
  | check value = while check update (update value)  -- Recursively updating the value while the condition is true
  | otherwise = value                                -- Returning the value when the condition is false

------------------------------------------------------------------------------
-- Ex 8
-- Defining a function to apply a function that returns an Either type until a Left value is encountered.
-- If the result is Left, it is returned; otherwise, the function is applied again to the Right value.
whileRight :: (a -> Either b a) -> a -> b
whileRight check x = case check x of
                       Left b  -> b                    -- Returning b if the result is Left
                       Right a -> whileRight check a   -- Recursively applying the function if the result is Right

------------------------------------------------------------------------------
-- Ex 9
-- Defining a function to combine pairs of strings from a list such that their concatenated length equals a given value.
-- The function returns a list of such combined strings.
joinToLength :: Int -> [String] -> [String]
joinToLength len strs = [x ++ y | x <- strs, y <- strs, length (x ++ y) == len]  -- Combining strings to match the given length

------------------------------------------------------------------------------
-- Ex 10
-- Defining an infix function to concatenate the first elements of two lists.
-- If both lists are empty, the result is an empty list.
-- If one list is empty, the result is the first element of the non-empty list.
-- Otherwise, it returns a list containing the first elements of both lists.
(+|+) :: [a] -> [a] -> [a]
[] +|+ [] = []
(x:_) +|+ [] = [x]            -- Returning the first element of the first list if the second list is empty
[] +|+ (y:_) = [y]            -- Returning the first element of the second list if the first list is empty
(x:_) +|+ (y:_) = [x, y]      -- Returning the first elements of both lists

------------------------------------------------------------------------------
-- Ex 11
-- Defining a function to sum the values from a list of Either types, only summing the Right values.
-- The function uses the rights function to filter out the Right values and then calculates their sum.
sumRights :: [Either a Int] -> Int
sumRights = sum . rights
  where rights = map (\(Right x) -> x) . filter isRight  -- Summing the Right values in the list

------------------------------------------------------------------------------
-- Ex 12
-- Defining a function to compose a list of functions into a single function.
-- The resulting function applies the functions from the list in sequence.
multiCompose :: [a -> a] -> a -> a
multiCompose fs = foldr (.) id fs  -- Composing a list of functions

------------------------------------------------------------------------------
-- Ex 13
-- Defining a function to apply a list of functions to an argument and combine the results using another function.
-- The function first applies each function in the list to the argument and then combines the results.
multiApp :: ([b] -> c) -> [a -> b] -> a -> c
multiApp f gs x = f (map ($ x) gs)  -- Applying a list of functions to an argument and combining the results

------------------------------------------------------------------------------
-- Ex 14
-- Defining a function to interpret a list of commands that modify coordinates (x, y).
-- The function processes commands such as "up", "down", "left", "right", "printX", and "printY".
-- It returns a list of strings representing the output of "printX" and "printY" commands.
interpreter :: [String] -> [String]
interpreter = go 0 0
  where
    go _ _ [] = []
    go x y (cmd:cmds) = case cmd of
      "up"     -> go x (y + 1) cmds         -- Moving up by increasing y
      "down"   -> go x (y - 1) cmds         -- Moving down by decreasing y
      "left"   -> go (x - 1) y cmds         -- Moving left by decreasing x
      "right"  -> go (x + 1) y cmds         -- Moving right by increasing x
      "printX" -> show x : go x y cmds      -- Printing the current x coordinate
      "printY" -> show y : go x y cmds      -- Printing the current y coordinate
      _        -> go x y cmds               -- Ignoring unknown commands
