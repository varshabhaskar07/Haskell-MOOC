module Set10a where

import Data.Char
import Data.List

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1

-- Double each element in the list by concatenating it with itself.
doublify :: [a] -> [a]
doublify = concatMap (\x -> [x, x])

------------------------------------------------------------------------------
-- Ex 2 

-- Interleave two lists by alternating their elements.
-- If one list is shorter, the remaining elements of the longer list are appended.
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

------------------------------------------------------------------------------
-- Ex 3 

-- Deal cards to players in a round-robin fashion.
-- Each card is assigned to a player in a cyclic order.
deal :: [String] -> [String] -> [(String, String)]
deal players cards = zip cards (cycle players)

------------------------------------------------------------------------------
-- Ex 4 

-- Calculate running averages of a list of numbers.
-- Each element in the result list is the average of all elements up to that point.
averages :: [Double] -> [Double]
averages = go 0 0
  where
    go _ _ [] = []
    go sum count (x:xs) = let sum' = sum + x
                              count' = count + 1
                          in (sum' / fromIntegral count') : go sum' count' xs

------------------------------------------------------------------------------
-- Ex 5 

-- Alternate elements of two lists with a separator.
-- The separator is inserted between the lists and between each repetition.
alternate :: [a] -> [a] -> a -> [a]
alternate xs ys z = xs ++ [z] ++ ys ++ [z] ++ alternate xs ys z

------------------------------------------------------------------------------
-- Ex 6 

-- Check if a list has at least a certain length.
-- Returns True if the list contains at least 'n' elements, otherwise False.
lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast 0 _      = True
lengthAtLeast _ []     = False
lengthAtLeast n (_:xs) = lengthAtLeast (n-1) xs

------------------------------------------------------------------------------
-- Ex 7

-- Split a list into chunks of a specified size.
-- Each chunk is of size 'n', and the list is divided into consecutive chunks.
chunks :: Int -> [a] -> [[a]]
chunks n xs
  | lengthAtLeast n xs = take n xs : chunks n (tail xs)
  | otherwise = []

------------------------------------------------------------------------------
-- Ex 8 

-- Define a new type for case-insensitive string comparison.
-- The IgnoreCase type wraps a string and ignores case when comparing.
newtype IgnoreCase = IgnoreCase String

instance Eq IgnoreCase where
  (IgnoreCase s1) == (IgnoreCase s2) = map toLower s1 == map toLower s2

ignorecase :: String -> IgnoreCase
ignorecase = IgnoreCase

------------------------------------------------------------------------------
-- Ex 9

-- Define a simple room-based maze game.
-- Rooms are described with a name and a list of possible directions leading to other rooms.

data Room = Room String [(String, Room)]

-- Get the description of a room.
describe :: Room -> String
describe (Room s _) = s

-- Move to a new room based on a given direction.
-- Return Nothing if the direction is invalid, otherwise return the new room.
move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

-- Play through the maze by following a list of directions.
-- The game progresses room by room based on the directions provided.
play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) = case move room d of
  Nothing -> [describe room]
  Just r  -> describe room : play r ds

-- Define different mazes for the game.
maze1, maze2, maze3 :: Room
maze1 = Room "Maze" [("Left", maze2), ("Right", maze3)]
maze2 = Room "Deeper in the maze" [("Left", maze3), ("Right", maze1)]
maze3 = Room "Elsewhere in the maze" [("Left", maze1), ("Right", maze2)]

-- Start the maze game from maze1.
maze :: Room
maze = maze1
