module Set9a where

import Data.Char
import Data.List
import Data.Ord

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1
-- Calculate the total workload based on the number of exercises and hours per exercise.
-- Provide different responses based on the total hours:
--   - "Holy moly!" if the total hours exceed 100,
--   - "Piece of cake!" if the total hours are fewer than 10,
--   - "Ok." if the total hours are between 10 and 100 inclusive.

workload :: Int -> Int -> String
workload nExercises hoursPerExercise
  | totalHours > 100 = "Holy moly!"
  | totalHours < 10 = "Piece of cake!"
  | otherwise = "Ok."
  where
    totalHours = nExercises * hoursPerExercise

------------------------------------------------------------------------------
-- Ex 2
-- Echo a string with each character followed by a comma and space, 
-- except for the last character. For example, "hello" will be converted to "h, e, l, l, o".

echo :: String -> String
echo [] = ""
echo s = s ++ ", " ++ echo (tail s)

------------------------------------------------------------------------------
-- Ex 3
-- Count the number of valid strings in a list based on their length and character conditions.
-- A string is considered valid if its length is at least 6 and
-- either the character at position 2 is equal to the character at position 4,
-- or the character at position 3 is equal to the character at position 5.

countValid :: [String] -> Int
countValid = length . filter isValid
  where
    isValid s = (length s >= 6) && (s !! 2 == s !! 4 || s !! 3 == s !! 5)

------------------------------------------------------------------------------
-- Ex 4
-- Find the first element in a list that repeats consecutively.
-- Return Nothing if there are no consecutive repeating elements,
-- if the list is empty, or if it contains only one element.

repeated :: Eq a => [a] -> Maybe a
repeated [] = Nothing
repeated [x] = Nothing
repeated (x:y:xs)
  | x == y = Just x
  | otherwise = repeated (y:xs)

------------------------------------------------------------------------------
-- Ex 5
-- Sum up all successful results from a list of Either values.
-- Return Left "no data" if there are no successful results,
-- otherwise return the sum of the successful results.

sumSuccess :: [Either String Int] -> Either String Int
sumSuccess xs =
  let successes = [x | Right x <- xs]
  in if null successes then Left "no data" else Right (sum successes)

------------------------------------------------------------------------------
-- Ex 6
-- Manage a lock's state with functionality to open, close, and change the lock's code.
-- Check if the lock is open, open it with a correct code, lock it again, and change the code if it's unlocked.

data Lock = Locked String | Unlocked String
  deriving Show

aLock :: Lock
aLock = Locked "1234"

-- Check if the lock is open.
isOpen :: Lock -> Bool
isOpen (Unlocked _) = True
isOpen _ = False

-- Open the lock if the provided code matches the current code.
open :: String -> Lock -> Lock
open code (Locked c)
  | code == c = Unlocked c
  | otherwise = Locked c
open _ l = l

-- Lock the lock if it's currently unlocked.
lock :: Lock -> Lock
lock (Unlocked c) = Locked c
lock l = l

-- Change the lock's code if it's currently unlocked.
changeCode :: String -> Lock -> Lock
changeCode newCode (Unlocked _) = Unlocked newCode
changeCode _ l = l

------------------------------------------------------------------------------
-- Ex 7
-- Compare two Text values for equality after removing all spaces.
-- Implement an Eq instance to ignore spaces when comparing Text values.

data Text = Text String
  deriving Show

instance Eq Text where
  (Text s1) == (Text s2) = filter (not . isSpace) s1 == filter (not . isSpace) s2

------------------------------------------------------------------------------
-- Ex 8
-- Compose two lists of pairs into a new list of pairs by looking up values from the second list based on the first.
-- Create a list of pairs where the first element comes from the first list and the second element is looked up from the second list.

compose :: (Eq a, Eq b) => [(a,b)] -> [(b,c)] -> [(a,c)]
compose f g = [(a, c) | (a, b) <- f, Just c <- [lookup b g]]

------------------------------------------------------------------------------
-- Ex 9
-- Handle permutations of lists of integers.
-- Create an identity permutation, multiply two permutations, and permute a list based on a permutation.

type Permutation = [Int]

-- Generate the identity permutation of length n.
identity :: Int -> Permutation
identity n = [0 .. n - 1]

-- Multiply two permutations.
-- The resulting permutation maps each index according to the given permutations.
multiply :: Permutation -> Permutation -> Permutation
multiply p q = map (\i -> p !! (q !! i)) (identity (length p))

-- Permute a list according to a permutation.
permute :: Permutation -> [a] -> [a]
permute perm xs = map snd . sortOn fst $ zip perm xs
