module Set12 where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Monoid

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Increment all elements in a Functor by 1
incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll = fmap (+1)

------------------------------------------------------------------------------
-- Ex 2: Map a function over nested Functors with two levels
fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- Map a function over nested Functors with three levels
fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

------------------------------------------------------------------------------
-- Ex 3: Define a custom `Result` type and provide a Functor instance
data Result a = MkResult a | NoResult | Failure String
  deriving Show

-- Map a function over a `Result` type
instance Functor Result where
  fmap _ NoResult      = NoResult           -- Apply no transformation if NoResult
  fmap _ (Failure msg) = Failure msg        -- Apply no transformation if Failure
  fmap f (MkResult a)  = MkResult (f a)     -- Apply function f to the contained value

------------------------------------------------------------------------------
-- Ex 4: Define a custom `List` type and provide a Functor instance
data List a = Empty | LNode a (List a)
  deriving Show

-- Map a function over a custom `List` type
instance Functor List where
  fmap _ Empty = Empty                        -- Apply no transformation if Empty
  fmap f (LNode x xs) = LNode (f x) (fmap f xs) -- Apply function f to elements in the list

------------------------------------------------------------------------------
-- Ex 5: Define a custom `TwoList` type and provide a Functor instance
data TwoList a = TwoEmpty | TwoNode a a (TwoList a)
  deriving Show

-- Map a function over a custom `TwoList` type
instance Functor TwoList where
  fmap _ TwoEmpty = TwoEmpty                -- Apply no transformation if TwoEmpty
  fmap f (TwoNode x y xs) = TwoNode (f x) (f y) (fmap f xs) -- Apply function f to elements in the TwoList

------------------------------------------------------------------------------
-- Ex 6: Count occurrences of a specific element in a Foldable structure
count :: (Eq a, Foldable f) => a -> f a -> Int
count x = length . filter (== x) . toList
  -- Convert the Foldable structure to a list, filter by the element, and count occurrences

------------------------------------------------------------------------------
-- Ex 7: Find elements that are present in both Foldable structures
inBoth :: (Foldable f, Foldable g, Eq a) => f a -> g a -> [a]
inBoth xs ys = filter (`elem` toList ys) (toList xs)
  -- Convert both Foldable structures to lists, and find common elements

------------------------------------------------------------------------------
-- Ex 8: Make the custom `List` type an instance of Foldable
instance Foldable List where
  foldr _ z Empty = z                   -- Return the accumulator if Empty
  foldr f z (LNode x xs) = f x (foldr f z xs) -- Fold the list using foldr

------------------------------------------------------------------------------
-- Ex 9: Make the custom `TwoList` type an instance of Foldable
instance Foldable TwoList where
  foldr _ z TwoEmpty = z                        -- Return the accumulator if TwoEmpty
  foldr f z (TwoNode x y xs) = f x (f y (foldr f z xs)) -- Fold the TwoList using foldr

------------------------------------------------------------------------------
-- Ex 10: Define a `Fun` type with a Functor instance
data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

-- Map a function over a `Fun` type
instance Functor Fun where
  fmap g (Fun f) = Fun (g . f)
  -- Compose the function g with the function f contained in Fun

------------------------------------------------------------------------------
-- Ex 11: Define a `Tree` type and provide Functor and Foldable instances
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

-- Map a function over a `Tree` type
instance Functor Tree where
  fmap _ Leaf = Leaf                        -- Apply no transformation if Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    -- Apply function f to the node and recursively to left and right subtrees

-- Sum elements in a `Tree` using Monoid
sumTree :: Monoid m => Tree m -> m
sumTree Leaf = mempty
sumTree (Node x l r) = sumTree l <> x <> sumTree r
  -- Sum elements in the tree by combining the sums of left and right subtrees with the node value

-- Make the `Tree` type an instance of Foldable
instance Foldable Tree where
  foldMap f t = sumTree (fmap f t)
    -- Use foldMap to map function f over the tree and then sum the results
