module Set11b where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Append all strings in the list to the value of the IORef
appendAll :: IORef String -> [String] -> IO ()
appendAll ref strs = do
  -- Read the current value from the IORef
  current <- readIORef ref
  -- Concatenate all strings in the list and append them to the current value
  let newValue = current ++ concat strs
  -- Write the new value back to the IORef
  writeIORef ref newValue

------------------------------------------------------------------------------
-- Ex 2: Swap values between two IORefs
swapIORefs :: IORef a -> IORef a -> IO ()
swapIORefs ref1 ref2 = do
  -- Read the values from both IORefs
  value1 <- readIORef ref1
  value2 <- readIORef ref2
  -- Write the value of the second IORef to the first IORef
  writeIORef ref1 value2
  -- Write the value of the first IORef to the second IORef
  writeIORef ref2 value1

------------------------------------------------------------------------------
-- Ex 3: Execute a nested IO action twice
doubleCall :: IO (IO a) -> IO a
doubleCall op = do
  -- Unwrap the nested IO action
  nestedOp <- op
  -- Execute the nested IO action
  nestedOp

------------------------------------------------------------------------------
-- Ex 4: Compose two IO actions
compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
compose op1 op2 c = do
  -- Apply the second IO action to the input
  a <- op2 c
  -- Apply the first IO action to the result of the second IO action
  op1 a

------------------------------------------------------------------------------
-- Ex 5: Fetch all lines from a file handle
hFetchLines :: Handle -> IO [String]
hFetchLines handle = do
  -- Check if the end of the file has been reached
  isEndOfFile <- hIsEOF handle
  if isEndOfFile
    then return []  -- Return an empty list if at end of file
    else do
      -- Read the current line from the handle
      line <- hGetLine handle
      -- Recursively fetch the remaining lines
      remainingLines <- hFetchLines handle
      -- Return the list of lines including the current line
      return (line : remainingLines)

------------------------------------------------------------------------------
-- Ex 6: Select specific lines from a file handle based on given indices
hSelectLines :: Handle -> [Int] -> IO [String]
hSelectLines handle indices = do
  -- Fetch all lines from the file handle
  allLines <- hFetchLines handle
  -- Select lines based on the provided indices, adjusting for 1-based indexing
  return [allLines !! (i - 1) | i <- indices, i > 0, i <= length allLines]

------------------------------------------------------------------------------
-- Ex 7: Manage state transitions based on commands
counter :: (String, Integer) -> (Bool, String, Integer)
counter ("inc", n)   = (True, "done", n + 1)    -- Increment the counter
counter ("print", n) = (True, show n, n)        -- Print the current counter value
counter ("quit", n)  = (False, "bye bye", n)   -- Terminate interaction

-- Interact with the user and update the state based on commands
interact' :: ((String, st) -> (Bool, String, st)) -> st -> IO st
interact' handler initialState = do
  -- Read user input
  input <- getLine
  -- Process the input with the handler to get the new state
  let (shouldContinue, output, newState) = handler (input, initialState)
  -- Print the output message
  putStrLn output
  -- Continue interacting or return the final state based on the continuation flag
  if shouldContinue
    then interact' handler newState
    else return newState
