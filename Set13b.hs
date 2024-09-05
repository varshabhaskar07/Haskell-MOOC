{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set13b where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List

------------------------------------------------------------------------------
-- Ex 1: Define the `test` function and the `ifM` combinator

-- Test if the current state integer is less than 10
test :: State Int Bool
test = (< 10) <$> get

-- Conditional execution based on a monadic boolean value
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond opThen opElse = do
  condition <- cond
  if condition then opThen else opElse

------------------------------------------------------------------------------
-- Ex 2: Work with lists and monads

-- Safely divide two Doubles, returning Nothing if division by zero occurs
safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0.0 = Nothing
safeDiv x y = Just (x / y)

-- Conditionally increment the state based on a boolean flag
perhapsIncrement :: Bool -> Int -> State Int ()
perhapsIncrement True x = modify (+x)
perhapsIncrement False _ = return ()

-- Map a function over two lists using monadic operations
mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 _ [] _ = return []
mapM2 _ _ [] = return []
mapM2 op (x:xs) (y:ys) = do
  z <- op x y
  zs <- mapM2 op xs ys
  return (z : zs)

------------------------------------------------------------------------------
-- Ex 3: Find paths in a maze

-- Define a maze as a list of nodes with their neighbors
maze1 :: [(String,[String])]
maze1 = [("Entry", ["Pit", "Corridor 1"])
        ,("Pit", [])
        ,("Corridor 1", ["Entry", "Dead end"])
        ,("Dead end", ["Corridor 1"])
        ,("Corridor 2", ["Corridor 3"])
        ,("Corridor 3", ["Corridor 2"])]

-- Visit nodes in the maze and record visited nodes
visit :: [(String,[String])] -> String -> State [String] ()
visit maze place = do
  visited <- get
  if place `elem` visited
    then return ()
    else do
      put (place : visited)
      let neighbors = maybe [] id (lookup place maze)
      mapM_ (visit maze) neighbors

-- Check if there is a path between two places in the maze
path :: [(String,[String])] -> String -> String -> Bool
path maze place1 place2 = place2 `elem` execState (visit maze place1) []

------------------------------------------------------------------------------
-- Ex 4: Find pairs of integers that sum to an integer in a list

-- Find pairs (i, j) from list `ks` that sum to a value in list `ns`
findSum2 :: [Int] -> [Int] -> [(Int, Int, Int)]
findSum2 ks ns = [(i, j, n) | i <- ks, j <- ks, let n = i + j, n `elem` ns]

------------------------------------------------------------------------------
-- Ex 5: Generate all possible sums from a list of integers

-- Calculate all possible sums of subsets of the given list
allSums :: [Int] -> [Int]
allSums xs = nub $ do
  included <- replicateM (length xs) [True, False]
  return $ sum $ map snd $ filter fst $ zip included xs

------------------------------------------------------------------------------
-- Ex 6: Handle bounded sums and avoid repeated sums

-- Sum elements of the list while ensuring the sum does not exceed `k`
sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

-- Helper function for `sumBounded` to add an element while checking the bound
f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x = let newSum = acc + x in if newSum > k then Nothing else Just newSum

-- Sum elements of the list, avoiding summing duplicates
sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 0 xs) []

-- Helper function for `sumNotTwice` to add an element if it hasn't been seen
f2 :: Int -> Int -> State [Int] Int
f2 acc x = do
  seen <- get
  if x `elem` seen
    then return acc
    else do
      put (x : seen)
      return (acc + x)

------------------------------------------------------------------------------
-- Ex 7: Implement a custom `Result` type with Functor, Applicative, and Monad instances

-- Define a result type with possible values: successful, no result, or failure
data Result a = MkResult a | NoResult | Failure String deriving (Show, Eq)

-- Implement Functor for `Result`
instance Functor Result where
  fmap _ NoResult = NoResult
  fmap _ (Failure msg) = Failure msg
  fmap f (MkResult a) = MkResult (f a)

-- Implement Applicative for `Result`
instance Applicative Result where
  pure = return
  (<*>) = ap

-- Implement Monad for `Result`
instance Monad Result where
  return = MkResult
  MkResult a >>= f = f a
  NoResult >>= _ = NoResult
  Failure msg >>= _ = Failure msg

------------------------------------------------------------------------------
-- Ex 8: Implement a state monad with logging

-- Define a state monad with logging capability
data SL a = SL (Int -> (a, Int, [String]))

-- Run an `SL` operation with a starting state
runSL :: SL a -> Int -> (a, Int, [String])
runSL (SL f) state = f state

-- Create a log message
msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((), s, [msg]))

-- Fetch the current state
getSL :: SL Int
getSL = SL (\s -> (s, s, []))

-- Overwrite the state with a new value
putSL :: Int -> SL ()
putSL s' = SL (\s -> ((), s', []))

-- Modify the state using a function
modifySL :: (Int -> Int) -> SL ()
modifySL f = SL (\s -> ((), f s, []))

-- Implement Functor for `SL`
instance Functor SL where
  fmap f (SL g) = SL (\s -> let (a, s', log) = g s in (f a, s', log))

-- Implement Applicative for `SL`
instance Applicative SL where
  pure x = SL (\s -> (x, s, []))
  (SL f) <*> (SL g) = SL (\s ->
    let (fab, s1, log1) = f s
        (a, s2, log2) = g s1
    in (fab a, s2, log1 ++ log2))

-- Implement Monad for `SL`
instance Monad SL where
  return x = SL (\s -> (x, s, []))
  (SL f) >>= g = SL (\s ->
    let (a, s1, log1) = f s
        (SL h) = g a
        (b, s2, log2) = h s1
    in (b, s2, log1 ++ log2))

------------------------------------------------------------------------------
-- Ex 9: Create a counter with IO operations

-- Create a counter with increment and get operations
mkCounter :: IO (IO (), IO Int)
mkCounter = do
  ref <- newIORef 0
  let inc = modifyIORef ref (+1)
      get = readIORef ref
  return (inc, get)
