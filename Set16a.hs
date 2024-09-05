module Set16a where

import Mooc.Todo
import Test.QuickCheck

import Data.List

------------------------------------------------------------------------------
-- Exercise 1: Checking if the List is Sorted

-- Property test to check if a list is sorted in non-decreasing order.
-- The list is considered sorted if for every adjacent pair of elements, the first element is less than or equal to the second.
isSorted :: (Show a, Ord a) => [a] -> Property
isSorted xs = property $ all (uncurry (<=)) $ zip xs (tail xs)

------------------------------------------------------------------------------
-- Exercise 2: Verifying that the Sum of Counts Equals the Length of the List

-- Property test to ensure that the sum of counts in the output list equals the length of the input list.
sumIsLength :: Show a => [a] -> [(a, Int)] -> Property
sumIsLength input output = property $ sum (map snd output) == length input

-- A simple frequency function that counts the occurrences of each element in a list.
freq1 :: Eq a => [a] -> [(a, Int)]
freq1 [] = []
freq1 [x] = [(x,1)]
freq1 (x:y:xs) = [(x,1),(y,length xs + 1)]

------------------------------------------------------------------------------
-- Exercise 3: Checking if Each Input Element is Represented in the Output

-- Property test to ensure that each element from the input list is represented in the output list.
inputInOutput :: (Show a, Eq a) => [a] -> [(a, Int)] -> Property
inputInOutput input output = forAll (elements input) $ \x ->
  property $ any (\(y, _) -> y == x) output

-- A frequency function where each element is counted as 1, regardless of its actual count.
freq2 :: Eq a => [a] -> [(a, Int)]
freq2 xs = map (\x -> (x,1)) xs

------------------------------------------------------------------------------
-- Exercise 4: Ensuring Each Output Element's Count Matches the Input's Count

-- Property test to verify that the count of each element in the output list matches its count in the input list.
outputInInput :: (Show a, Eq a) => [a] -> [(a, Int)] -> Property
outputInInput input output = conjoin $ map checkElem output
  where
    checkElem (x, n) = property $ length (filter (== x) input) == n

-- A frequency function that counts the occurrences of each element in the list.
freq3 :: Eq a => [a] -> [(a, Int)]
freq3 [] = []
freq3 (x:xs) = [(x, 1 + length (filter (== x) xs))]

------------------------------------------------------------------------------
-- Exercise 5: Testing the Properties of Frequency Functions

-- Property test to validate the properties of frequency functions.
-- Ensures that the frequency function satisfies the properties: sum of counts equals length of list, 
-- all input elements are represented in output, and all output counts match input counts.
frequenciesProp :: ([Char] -> [(Char, Int)]) -> NonEmptyList Char -> Property
frequenciesProp freq (NonEmpty input) = 
  let output = freq input
  in  conjoin [ sumIsLength input output
              , inputInOutput input output
              , outputInInput input output
              ]

-- A complete frequency function that counts occurrences of each element using partitioning.
frequencies :: Eq a => [a] -> [(a, Int)]
frequencies [] = []
frequencies (x:ys) = (x, length xs) : frequencies others
  where
     (xs, others) = partition (== x) (x:ys)

------------------------------------------------------------------------------
-- Exercise 6: Generating a Sorted List of Integers

-- Generates a sorted list of integers with a length between 3 and 5.
-- Each integer is between 0 and 10.
genList :: Gen [Int]
genList = do
  len <- choose (3, 5)
  xs <- vectorOf len (choose (0, 10))
  return $ sort xs

------------------------------------------------------------------------------
-- Exercise 7: Defining Data Types for Expressions and Generating Arbitrary Values

-- Data type representing an argument in an expression, which can be a number or a variable.
data Arg = Number Int | Variable Char
  deriving (Show, Eq)

-- Data type representing an expression, which can be either addition or subtraction of two arguments.
data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

-- Arbitrary instance for generating random `Arg` values.
instance Arbitrary Arg where
  arbitrary = oneof [ Number <$> choose (0, 10)
                    , Variable <$> elements "ab"
                    ]

-- Arbitrary instance for generating random `Expression` values.
instance Arbitrary Expression where
  arbitrary = oneof [ Plus <$> arbitrary <*> arbitrary
                    , Minus <$> arbitrary <*> arbitrary
                    ]
