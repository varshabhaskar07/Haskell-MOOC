module Set11a where

import Control.Monad
import Data.List
import System.IO

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: I am Printing "HELLO" and "WORLD"

hello :: IO ()
hello = putStrLn "HELLO" >> putStrLn "WORLD"

------------------------------------------------------------------------------
-- Ex 2:  I am Greeting the user with the provided name

greet :: String -> IO ()
greet name = do
  putStrLn ("HELLO " ++ name)

------------------------------------------------------------------------------
-- Ex 3: I am  Reading the name from the user and greeting them

greet2 :: IO ()
greet2 = do
  name <- getLine
  greet name

------------------------------------------------------------------------------
-- Ex 4: I am  Reading a specified number of words and sorting them

readWords :: Int -> IO [String]
readWords 0 = return []
readWords n = do
  word <- getLine
  rest <- readWords (n - 1)
  return (sort (word : rest))

------------------------------------------------------------------------------
-- Ex 5:  I am Reading lines from the user until a condition is met

readUntil :: (String -> Bool) -> IO [String]
readUntil f = do
  line <- getLine
  if f line
    then return []
    else liftM2 (:) (return line) (readUntil f)

------------------------------------------------------------------------------
-- Ex 6:  I am Printing a countdown from the given number to zero

countdownPrint :: Int -> IO ()
countdownPrint n
  | n < 0 = return ()
  | otherwise = do
      print n
      countdownPrint (n - 1)

------------------------------------------------------------------------------
-- Ex 7:  I am Summing up a specified number of inputs and printing the running total

isums :: Int -> IO Int
isums n = loop n 0
  where
    loop 0 sum = return sum
    loop k sum = do
      x <- readLn
      let newSum = sum + x
      print newSum
      loop (k - 1) newSum

------------------------------------------------------------------------------
-- Ex 8: I am  Executing an action if a given condition is true

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
  c <- cond
  when c op

------------------------------------------------------------------------------
-- Ex 9: I am  Repeatedly executing an action while a condition remains true

while :: IO Bool -> IO () -> IO ()
while cond op = cond >>= \c -> when c (op >> while cond op)

------------------------------------------------------------------------------
-- Ex 10:  I am  Debugging by printing a message before and after executing an action

debug :: String -> IO a -> IO a
debug s op = do
  putStrLn s
  result <- op
  putStrLn s
  return result
