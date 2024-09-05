module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup

------------------------------------------------------------------------------
-- Ex 1

-- Defining data types for Distance, Time, and Velocity.
data Distance = Distance Double
  deriving (Show, Eq)

data Time = Time Double
  deriving (Show, Eq)

data Velocity = Velocity Double
  deriving (Show, Eq)

-- Calculating velocity by dividing distance by time.
velocity :: Distance -> Time -> Velocity
velocity (Distance d) (Time t) = Velocity (d / t)

-- Calculating distance traveled by multiplying velocity by time.
travel :: Velocity -> Time -> Distance
travel (Velocity v) (Time t) = Distance (v * t)

------------------------------------------------------------------------------
-- Ex 2

-- Defining a 'Set' data type which wraps a list of elements.
data Set a = Set [a]
  deriving (Show, Eq)

-- Initializing an empty set.
emptySet :: Set a
emptySet = Set []

-- Checking if a given element is a member of the set.
member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

-- Adding an element to the set, ensuring the set remains sorted.
add :: Ord a => a -> Set a -> Set a
add x (Set xs) = Set (insert x xs)
  where
    insert x [] = [x]
    insert x ys@(y:ys')
      | x == y = ys
      | x < y = x : ys
      | otherwise = y : insert x ys'

------------------------------------------------------------------------------
-- Ex 3

-- Defining an 'Event' type with possible baking events.
data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq, Show)

-- Defining a 'State' type to represent the state of the baking process.
data State = Start | Add | Sweet | Dough | Batter | Mixed | Error | Finished
  deriving (Eq, Show)

-- Defining the state transitions based on events during the baking process.
step :: State -> Event -> State
step Finished _     = Finished
step Error _        = Error
step Start AddEggs  = Add
step Add AddFlour   = Dough
step Dough AddSugar = Batter
step Add AddSugar   = Sweet
step Sweet AddFlour = Batter
step Batter Mix     = Mixed
step Mixed Bake     = Finished
step _ _            = Error

-- Determining the final state by iterating through a list of events.
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4

-- Calculating the average of a 'NonEmpty' list.
average :: Fractional a => NonEmpty a -> a
average (x :| xs) = sum (x:xs) / fromIntegral (length (x:xs))

------------------------------------------------------------------------------
-- Ex 5 

-- Reversing a 'NonEmpty' list.
reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) = let (y:ys) = reverse (x:xs) in y :| ys

------------------------------------------------------------------------------
-- Ex 6

-- Defining the 'Semigroup' instance for 'Distance' to combine distances by addition.
instance Semigroup Distance where
  Distance d1 <> Distance d2 = Distance (d1 + d2)

-- Defining the 'Semigroup' instance for 'Time' to combine times by addition.
instance Semigroup Time where
  Time t1 <> Time t2 = Time (t1 + t2)

-- Defining the 'Semigroup' instance for 'Velocity' to combine velocities by addition.
instance Semigroup Velocity where
  Velocity v1 <> Velocity v2 = Velocity (v1 + v2)

------------------------------------------------------------------------------
-- Ex 7 

-- Defining the 'Semigroup' instance for 'Set' to combine sets by merging them and removing duplicates.
instance Ord a => Semigroup (Set a) where
  Set xs <> Set ys = Set (union xs ys)
    where
      union [] ys = ys
      union xs [] = xs
      union (x:xs) (y:ys)
        | x == y = x : union xs ys
        | x < y = x : union xs (y:ys)
        | otherwise = y : union (x:xs) ys

-- Defining the 'Monoid' instance for 'Set' with the empty set as the identity element.
instance Ord a => Monoid (Set a) where
  mempty = emptySet

------------------------------------------------------------------------------
-- Ex 8

-- Defining various arithmetic operations.
data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
  deriving Show

-- Computing the result of an 'Operation1'.
compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i + j
compute1 (Subtract1 i j) = i - j
compute1 (Multiply1 i j) = i * j

-- Rendering 'Operation1' as a string.
show1 :: Operation1 -> String
show1 (Add1 i j) = show i ++ "+" ++ show j
show1 (Subtract1 i j) = show i ++ "-" ++ show j
show1 (Multiply1 i j) = show i ++ "*" ++ show j

-- Defining data types for arithmetic operations with a different structure.
data Add2 = Add2 Int Int
  deriving Show

data Subtract2 = Subtract2 Int Int
  deriving Show

data Multiply2 = Multiply2 Int Int
  deriving Show

-- Defining an 'Operation2' typeclass with methods for computing and displaying operations.
class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

-- Implementing 'Operation2' for 'Add2'.
instance Operation2 Add2 where
  compute2 (Add2 i j) = i + j
  show2 (Add2 i j) = show i ++ "+" ++ show j

-- Implementing 'Operation2' for 'Subtract2'.
instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i - j
  show2 (Subtract2 i j) = show i ++ "-" ++ show j

-- Implementing 'Operation2' for 'Multiply2'.
instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i * j
  show2 (Multiply2 i j) = show i ++ "*" ++ show j

------------------------------------------------------------------------------
-- Ex 9

-- Defining a 'PasswordRequirement' data type with various requirements for a valid password.
data PasswordRequirement =
    MinimumLength Int               -- Minimum length requirement
  | ContainsSome String            -- Must contain at least one of the specified characters
  | DoesNotContain String          -- Must not contain any of the specified characters
  | And PasswordRequirement PasswordRequirement -- Logical AND of two requirements
  | Or PasswordRequirement PasswordRequirement  -- Logical OR of two requirements
  deriving Show

-- Checking if a password meets the specified requirements.
passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed password (MinimumLength len) = length password >= len
passwordAllowed password (ContainsSome chars) = any (`elem` chars) password
passwordAllowed password (DoesNotContain chars) = all (`notElem` chars) password
passwordAllowed password (And req1 req2) = passwordAllowed password req1 && passwordAllowed password req2
passwordAllowed password (Or req1 req2) = passwordAllowed password req1 || passwordAllowed password req2

------------------------------------------------------------------------------
-- Ex 10

-- Defining an 'Arithmetic' data type for literals and operations.
data Arithmetic = Literal Integer
                | Operation String Arithmetic Arithmetic
  deriving Show

-- Creating a literal value in the 'Arithmetic' type.
literal :: Integer -> Arithmetic
literal = Literal

-- Creating an operation involving two 'Arithmetic' values.
operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = Operation

-- Evaluating an 'Arithmetic' expression to get the result.
evaluate :: Arithmetic -> Integer
evaluate (Literal n) = n
evaluate (Operation "+" a1 a2) = evaluate a1 + evaluate a2
evaluate (Operation "*" a1 a2) = evaluate a1 * evaluate a2

-- Rendering an 'Arithmetic' expression as a string.
render :: Arithmetic -> String
render (Literal n) = show n
render (Operation op a1 a2) = "(" ++ render a1 ++ op ++ render a2 ++ ")"
