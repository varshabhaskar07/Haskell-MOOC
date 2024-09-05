module Set5b where

import Mooc.Todo

-- Defining a binary tree data type with values of type `a`.
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1

-- Getting the value at the root of the tree.
valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node value _ _) = Just value

------------------------------------------------------------------------------
-- Ex 2

-- Calculating the size of the tree, which is the number of nodes.
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

------------------------------------------------------------------------------
-- Ex 3

-- Finding the maximum value in a tree of integers.
treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax (Node value left right) = max value (max (treeMax left) (treeMax right))

------------------------------------------------------------------------------
-- Ex 4

-- Checking if all values in the tree satisfy a given condition.
allValues :: (a -> Bool) -> Tree a -> Bool
allValues _ Empty = True
allValues condition (Node value left right) = 
  condition value && allValues condition left && allValues condition right

------------------------------------------------------------------------------
-- Ex 5

-- Applying a function to all values in the tree, creating a new tree with the results.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

------------------------------------------------------------------------------
-- Ex 6

-- Removing all occurrences of a given value from the tree.
cull :: Eq a => a -> Tree a -> Tree a
cull _ Empty = Empty
cull val (Node value left right)
  | val == value = Empty
  | otherwise = Node value (cull val left) (cull val right)

------------------------------------------------------------------------------
-- Ex 7

-- Checking if the tree is ordered as a binary search tree.
isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node value left right) = 
  allValues (< value) left && allValues (> value) right && isOrdered left && isOrdered right

------------------------------------------------------------------------------
-- Ex 8

-- Walking through the tree according to a list of steps and returning the value at the destination.
data Step = StepL | StepR
  deriving (Show, Eq)

walk :: [Step] -> Tree a -> Maybe a
walk [] (Node value _ _) = Just value
walk _ Empty = Nothing
walk (StepL:steps) (Node _ left _) = walk steps left
walk (StepR:steps) (Node _ _ right) = walk steps right

------------------------------------------------------------------------------
-- Ex 9

-- Setting a new value at a specific position in the tree, according to a list of steps.
set :: [Step] -> a -> Tree a -> Tree a
set [] newVal (Node _ left right) = Node newVal left right
set [] newVal Empty = Empty
set (StepL:steps) newVal (Node value left right) = Node value (set steps newVal left) right
set (StepR:steps) newVal (Node value left right) = Node value left (set steps newVal right)
set _ _ tree = tree

------------------------------------------------------------------------------
-- Ex 10

-- Searching for a value in the tree and returning the path to it as a list of steps.
search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search target (Node value left right)
  | target == value = Just []
  | otherwise = case search target left of
      Just steps -> Just (StepL : steps)
      Nothing -> case search target right of
        Just steps -> Just (StepR : steps)
        Nothing -> Nothing
