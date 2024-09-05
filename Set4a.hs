module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1

-- Defining a function to check if all elements in a list are equal.
-- The function returns True if all elements are the same; otherwise, it returns False.
allEqual :: Eq a => [a] -> Bool
allEqual [] = True  -- An empty list is considered to have all equal elements
allEqual (x:xs) = all (== x) xs  -- Check if all elements in the list are equal to the first element

------------------------------------------------------------------------------
-- Ex 2 

-- Defining a function to check if all elements in a list are distinct.
-- The function returns True if all elements are unique; otherwise, it returns False.
distinct :: Eq a => [a] -> Bool
distinct xs = length xs == length (nub xs)  -- Compare the length of the list to the length of the list with duplicates removed

------------------------------------------------------------------------------
-- Ex 3

-- Defining a function to find the middle element among three comparable values.
-- The function returns the second element when the values are sorted in ascending order.
middle :: Ord a => a -> a -> a -> a
middle x y z = sort [x, y, z] !! 1  -- Sort the list and select the middle element

------------------------------------------------------------------------------
-- Ex 4 

-- Defining a function to calculate the range (difference between the maximum and minimum values) of a list of numeric values.
rangeOf :: (Ord a, Num a) => [a] -> a
rangeOf xs = maximum xs - minimum xs  -- Subtract the minimum value from the maximum value

------------------------------------------------------------------------------
-- Ex 5

-- Defining a function to find the longest list among a list of lists.
-- The function returns the list with the greatest length.
longest :: Ord a => [[a]] -> [a]
longest (x:[]) = x  -- If there is only one list, return it
longest (x:y:xs)
      | length x > length y  = longest (x:xs)  -- Recursively compare lengths of lists
      | length x < length y  = longest (y:xs)
      | otherwise            = if x < y
                                then longest (x:xs)
                                else longest (y:xs)

------------------------------------------------------------------------------
-- Ex 6

-- Defining a function to increment the value associated with a given key in a list of key-value pairs.
-- The function returns a new list with the value for the specified key incremented by 1.
incrementKey :: (Eq k, Num v) => k -> [(k, v)] -> [(k, v)]
incrementKey _ [] = []  -- Return an empty list if the input list is empty
incrementKey key ((k, v):kvs)
  | key == k = (k, v + 1) : incrementKey key kvs  -- Increment value for the matching key
  | otherwise = (k, v) : incrementKey key kvs  -- Keep value unchanged for non-matching keys

------------------------------------------------------------------------------
-- Ex 7

-- Defining a function to compute the average of a list of fractional numbers.
-- The function returns the average value of the elements in the list.
average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)  -- Compute the average by dividing the sum by the number of elements

------------------------------------------------------------------------------
-- Ex 8

-- Defining a function to determine the winner between two players based on their scores stored in a map.
-- The function returns the name of the player with the higher score.
winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2 =
  case (Map.lookup player1 scores, Map.lookup player2 scores) of
    (Just score1, Just score2) -> if score1 >= score2 then player1 else player2  -- Compare scores if both players are present
    (Just _, Nothing) -> player1  -- Return player1 if only player1 has a score
    (Nothing, Just _) -> player2  -- Return player2 if only player2 has a score
    (Nothing, Nothing) -> player1  -- Default case, should not happen ideally (both players not in the map)

------------------------------------------------------------------------------
-- Ex 9

-- Defining a function to construct a frequency map of elements from a list.
-- The function returns a map where the keys are elements from the list and the values are their respective counts.
freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs = Map.fromListWith (+) . map (\x -> (x, 1))  -- Create a frequency map by counting occurrences of each element

------------------------------------------------------------------------------
-- Ex 10

-- Defining a function to transfer an amount between two accounts in a bank map.
-- The function updates the balances of both accounts: subtracting the amount from the source account and adding it to the target account.
transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank
  | amount < 0 = bank  -- No transfer for negative amounts
  | Map.notMember from bank || Map.notMember to bank = bank  -- No transfer if either account is not found
  | Map.findWithDefault 0 from bank < amount = bank  -- No transfer if the source account does not have sufficient funds
  | otherwise = Map.adjust (\x -> x - amount) from $ Map.adjust (+ amount) to bank  -- Update balances for valid transfer

------------------------------------------------------------------------------
-- Ex 11:

-- Defining a function to swap elements at two indices in an array.
-- The function returns a new array with the elements at the specified indices swapped.
swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j), (j, arr ! i)]  -- Use the array update operator to swap elements

------------------------------------------------------------------------------
-- Ex 12

-- Defining a function to find the index of the maximum element in an array.
-- The function returns the index of the element with the maximum value.
maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = fst $ maximumBy (comparing snd) (assocs arr)  -- Find the index of the maximum value by comparing element values
