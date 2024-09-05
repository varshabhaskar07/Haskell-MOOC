{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1:

-- Define the ||| operator, which works like a logical OR.
-- The result is True if either of the operands is True.
(|||) :: Bool -> Bool -> Bool
x ||| y = case y of
  True  -> True  -- Result is True if y is True.
  False -> x     -- Result is the value of x if y is False.

------------------------------------------------------------------------------
-- Ex 2:

-- Calculate the length of a list of Booleans.
-- Counts each element in the list, regardless of whether it is True or False.
boolLength :: [Bool] -> Int
boolLength []     = 0                      -- Return 0 if the list is empty.
boolLength (x:xs) = case x of
  True  -> 1 + boolLength xs  -- Add 1 for True and continue with the rest of the list.
  False -> 1 + boolLength xs  -- Add 1 for False and continue with the rest of the list.

------------------------------------------------------------------------------
-- Ex 3:

-- Validate a value based on a predicate.
-- Always return the value unchanged, regardless of whether it satisfies the predicate.
validate :: (a -> Bool) -> a -> a
validate predicate value = if predicate value then value else value

------------------------------------------------------------------------------
-- Ex 4: 

-- Define the MySeq class with the myseq function.
-- The myseq function takes an argument of type a and a value of type b,
-- and returns a value of type b based on the argument of type a.

class MySeq a where
  myseq :: a -> b -> b

-- Implement MySeq for Bool.
-- Always return the second argument, regardless of the Bool value.
instance MySeq Bool where
  myseq b x = case b of
    True  -> x  -- Return x if b is True.
    False -> x  -- Return x if b is False.

-- Implement MySeq for Int.
-- Always return the second argument, regardless of the Int value.
instance MySeq Int where
  myseq n x = case n of
    0 -> x  -- Return x if n is 0.
    _ -> x  -- Return x for any other Int value.

-- Implement MySeq for lists.
-- Always return the second argument, regardless of whether the list is empty or not.
instance MySeq [a] where
  myseq xs x = case xs of
    []    -> x  -- Return x if the list is empty.
    (_:_) -> x  -- Return x if the list is non-empty.
