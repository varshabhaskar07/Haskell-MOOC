module Set9b where

import Mooc.Todo
import Data.List

--------------------------------------------------------------------------------
-- Ex 1

type Row   = Int
type Col   = Int
type Coord = (Row, Col)

-- Move to the next row while resetting the column to 1.
nextRow :: Coord -> Coord
nextRow (i, j) = (i + 1, 1)

-- Move to the next column while keeping the row the same.
nextCol :: Coord -> Coord
nextCol (i, j) = (i, j + 1)

--------------------------------------------------------------------------------
-- Ex 2

type Size = Int

-- Print the board with queens and empty spaces.
-- Represent queens with 'Q' and empty spaces with '.'.
prettyPrint :: Size -> [Coord] -> String
prettyPrint n queens = unlines [ [ if (r, c) `elem` queens then 'Q' else '.' | c <- [1..n] ] | r <- [1..n] ]

--------------------------------------------------------------------------------
-- Ex 3

-- Check if two coordinates are in the same row.
sameRow :: Coord -> Coord -> Bool
sameRow (i, _) (k, _) = i == k

-- Check if two coordinates are in the same column.
sameCol :: Coord -> Coord -> Bool
sameCol (_, j) (_, l) = j == l

-- Check if two coordinates are on the same diagonal.
sameDiag :: Coord -> Coord -> Bool
sameDiag (i, j) (k, l) = (i - j) == (k - l)

-- Check if two coordinates are on the same antidiagonal.
sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i, j) (k, l) = (i + j) == (k + l)

--------------------------------------------------------------------------------
-- Ex 4

type Candidate = Coord
type Stack     = [Coord]

-- Determine if placing a queen at a candidate position is dangerous.
-- A position is dangerous if it conflicts with any existing queen
-- in the same row, column, diagonal, or antidiagonal.
danger :: Candidate -> Stack -> Bool
danger (r, c) = any (\q -> sameRow q (r, c) || sameCol q (r, c) || sameDiag q (r, c) || sameAntidiag q (r, c))

--------------------------------------------------------------------------------
-- Ex 5

-- Print the board with queens and mark dangerous positions.
-- Represent queens with 'Q', dangerous positions with '#',
-- and empty spaces with '.'.
prettyPrint2 :: Size -> Stack -> String
prettyPrint2 n queens = unlines [ [ if (r, c) `elem` queens then 'Q' else if danger (r, c) queens then '#' else '.' | c <- [1..n] ] | r <- [1..n] ]

--------------------------------------------------------------------------------
-- Ex 6

-- Fix the first element of the stack by checking column bounds and danger.
-- If the first element's column is out of bounds or dangerous,
-- increment the column and try again. Return Nothing if no valid position is found.
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst n [] = Nothing
fixFirst n ((r, c):rest)
  | c > n = Nothing
  | danger (r, c) rest = fixFirst n ((r, c+1):rest)
  | otherwise = Just ((r, c):rest)

--------------------------------------------------------------------------------
-- Ex 7

-- Continue by moving to the next row and adding the updated coordinate to the stack.
continue :: Stack -> Stack
continue [] = []
continue s@((i,j):qs) = nextRow (i, j) : s

-- Backtrack by adjusting the column of the last stack element.
-- If the stack is empty or contains only one element, return an empty list.
backtrack :: Stack -> Stack
backtrack [] = []
backtrack ((_, _):qs) = case qs of
  [] -> []
  ((i, j):qs') -> (i, j+1):qs'

--------------------------------------------------------------------------------
-- Ex 8

-- Perform a step in the backtracking process.
-- Fix the first element of the stack, continue if successful,
-- or backtrack if no valid position is found.
step :: Size -> Stack -> Stack
step n s = case fixFirst n s of
  Just s' -> continue s'
  Nothing -> backtrack s

--------------------------------------------------------------------------------
-- Ex 9

-- Finish the solving process by recursively performing steps
-- and backtracking until a solution is found or no further progress can be made.
finish :: Size -> Stack -> Stack
finish n s
  | length s == n + 1 = tail s
  | otherwise = let nextStep = step n s
                in if nextStep == s
                   then s  
                   else finish n nextStep

-- Solve the N-Queens problem starting with the initial stack containing the first coordinate.
solve :: Size -> Stack
solve n = finish n [(1, 1)]
