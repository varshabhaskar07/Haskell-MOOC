module Set2a where

import Mooc.Todo
import Data.List

------------------------------------------------------------------------------
-- Ex 1

-- Defining a list of years
years :: [Int]
years = [1982, 2004, 2020]

------------------------------------------------------------------------------
-- Ex 2

-- Creating a function "takeFinal" that returns the final n elements of a list
-- If n is greater than the length of the list, it returns the entire list
takeFinal :: Int -> [a] -> [a]
takeFinal n xs = drop (length xs - min n (length xs)) xs

------------------------------------------------------------------------------
-- Ex 3

-- Creating a function "updateAt" that updates the element at index i in the list
-- with a new value x. The index i is zero-based.
updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

------------------------------------------------------------------------------
-- Ex 4

-- Creating a function "substring" that extracts a substring from index i to index j
-- from the given string s. The substring includes characters from index i up to, 
-- but not including, index j.
substring :: Int -> Int -> String -> String
substring i j s = take (j - i) (drop i s)

------------------------------------------------------------------------------
-- Ex 5

-- Creating a function "isPalindrome" that checks if a given string is a palindrome
-- A string is a palindrome if it reads the same forward and backward.
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

------------------------------------------------------------------------------
-- Ex 6

-- Creating a function "palindromify" that makes a string a palindrome by 
-- iteratively removing characters from both ends of the string until it becomes 
-- a palindrome or until no more characters can be removed.
palindromify :: String -> String
palindromify s
  | isPalindrome s = s
  | otherwise = palindromify (init (tail s))

------------------------------------------------------------------------------
-- Ex 7

-- Creating a function "safeDiv" that safely divides two integers. 
-- It returns Nothing if the divisor is zero, otherwise it returns the result of the division.
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y
  | y == 0    = Nothing
  | otherwise = Just (x `div` y)

------------------------------------------------------------------------------
-- Ex 8

-- Creating a function "greet" that generates a greeting message.
-- It optionally includes the last name if provided.
greet :: String -> Maybe String -> String
greet first Nothing = "Hello, " ++ first ++ "!"
greet first (Just last) = "Hello, " ++ first ++ " " ++ last ++ "!"

------------------------------------------------------------------------------
-- Ex 9

-- Creating a function "safeIndex" that safely accesses an element by index in a list.
-- It returns Nothing if the index is out of bounds or negative; otherwise, it returns
-- the element at the specified index.
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just (xs !! i)

------------------------------------------------------------------------------
-- Ex 10

-- Creating a function "eitherDiv" that performs division safely.
-- It returns an error message if the divisor is zero, otherwise it returns
-- the result of the division.
eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x y
  | y == 0    = Left (show x ++ "/" ++ show y)
  | otherwise = Right (x `div` y)

------------------------------------------------------------------------------
-- Ex 11

-- Creating a function "addEithers" that adds two Either values.
-- If both values are Right, it returns their sum as a Right value.
-- If either value is Left, it propagates the first encountered error message.
addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right x) (Right y) = Right (x + y)
addEithers (Left s) _          = Left s
addEithers _ (Left s)          = Left s
