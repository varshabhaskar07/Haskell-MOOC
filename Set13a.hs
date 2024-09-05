{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- Silencing an uninteresting warning

module Set13a where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map

import Examples.Bank

------------------------------------------------------------------------------
-- Ex 1: Define a custom operator to chain `Maybe` operations

-- Operator to chain `Maybe` operations
(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x  ?> f = f x

-- Read and validate names from a string
readNames :: String -> Maybe (String, String)
readNames s =
  split s
  ?> checkNumber
  ?> checkCapitals

-- Split the string into first and last names
split :: String -> Maybe (String, String)
split s = case words s of
  [for, sur] -> Just (for, sur)
  _          -> Nothing

-- Check that the names do not contain numbers
checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (for, sur)
  | any isDigit for || any isDigit sur = Nothing
  | otherwise                          = Just (for, sur)

-- Check that both names start with uppercase letters
checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (for, sur) =
  if isUpper (head for) && isUpper (head sur)
    then Just (for, sur)
    else Nothing

------------------------------------------------------------------------------
-- Ex 2: Determine the winner between two players based on scores

-- Determine the player with the higher or equal score
winner :: [(String, Int)] -> String -> String -> Maybe String
winner scores player1 player2 = do
  score1 <- lookup player1 scores
  score2 <- lookup player2 scores
  return $ if score1 >= score2 then player1 else player2

------------------------------------------------------------------------------
-- Ex 3: Calculate the sum of values at specified indices in a list

-- Calculate the sum of values at given indices
selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = foldr (\i acc -> (+) <$> acc <*> safeIndex xs i) (Just 0) is

-- Safely retrieve the value at a given index
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise               = Just (xs !! i)

------------------------------------------------------------------------------
-- Ex 4: Define a Logger type to accumulate log messages

-- Define a Logger type that stores log messages and a value
data Logger a = Logger [String] a
  deriving (Show, Eq)

-- Create a Logger with a message
msg :: String -> Logger ()
msg s = Logger [s] ()

-- Implement Functor for Logger
instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

-- Implement Monad for Logger
instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la ++ lb) b
    where Logger lb b = f a

-- Implement Applicative for Logger
instance Applicative Logger where
  pure = return
  (<*>) = ap

-- Count and log items that match a given predicate
countAndLog :: Show a => (a -> Bool) -> [a] -> Logger Int
countAndLog pred xs = do
  let (matching, count) = foldr (\x (logs, c) -> if pred x then (show x : logs, c + 1) else (logs, c)) ([], 0) xs
  mapM_ msg matching
  return count

------------------------------------------------------------------------------
-- Ex 5: Define an example bank with accounts and balances

-- Example bank with initial account balances
exampleBank :: Bank
exampleBank = (Bank (Map.fromList [("harry",10),("cedric",7),("ginny",1)]))

-- Retrieve the balance for a given account
balance :: String -> BankOp Int
balance accountName = BankOp $ \(Bank accounts) ->
  let bal = Map.findWithDefault 0 accountName accounts
  in (bal, Bank accounts)

------------------------------------------------------------------------------
-- Ex 6: Transfer funds from one account to another

-- Transfer funds between accounts
rob :: String -> String -> BankOp ()
rob from to = 
  balance from +> \bal -> 
  withdrawOp from bal +> \_ -> 
  depositOp to bal

------------------------------------------------------------------------------
-- Ex 7: Double and increment a state value

-- Update state by doubling the value and adding one
update :: State Int ()
update = modify (\x -> x * 2 + 1)

------------------------------------------------------------------------------
-- Ex 8: Process parentheses to check for matching

-- Process parentheses characters to track balance
paren :: Char -> State Int ()
paren '(' = do
  n <- get
  when (n >= 0) $ modify (+1)

paren ')' = do
  n <- get
  if n == 0
    then put (-1)
    else when (n > 0) $ modify (subtract 1)

-- Check if parentheses in a string are matched
parensMatch :: String -> Bool
parensMatch s = count == 0
  where (_, count) = runState (mapM_ paren s) 0

------------------------------------------------------------------------------
-- Ex 9: Count occurrences of a value in the state list

-- Count occurrences of a value in the list maintained in state
count :: Eq a => a -> State [(a,Int)] ()
count x = do
  xs <- get
  let (before, matchedAfter) = span ((/= x) . fst) xs
  put $ before ++ case matchedAfter of
    [] -> [(x, 1)]
    ((_, n) : after) -> (x, n + 1) : after

------------------------------------------------------------------------------
-- Ex 10: Count the total occurrences of elements in a list

-- Count the total occurrences of elements in a list using state
occurrences :: (Eq a) => [a] -> State [(a,Int)] Int
occurrences xs = do
  mapM_ count xs
  length <$> get
