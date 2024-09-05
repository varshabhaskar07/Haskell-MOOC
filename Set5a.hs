module Set5a where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1

-- Defining the `Vehicle` data type with different types of vehicles.
data Vehicle = Bike | Bus | Tram | Train
  deriving Show

------------------------------------------------------------------------------
-- Ex 2

-- Defining the `BusTicket` data type to represent different types of bus tickets.
data BusTicket = SingleTicket | MonthlyTicket String
  deriving Show

------------------------------------------------------------------------------
-- Ex 3

-- Defining the `ShoppingEntry` data type to represent an item in a shopping list.
data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

-- Creating a `ShoppingEntry` for three apples.
threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

-- Creating a `ShoppingEntry` for two bananas.
twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

-- Calculating the total price of a `ShoppingEntry`.
totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ price count) = price * fromIntegral count

-- Adding one more item to a `ShoppingEntry`.
buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry name price count) = MkShoppingEntry name price (count + 1)

------------------------------------------------------------------------------
-- Ex 4

-- Defining the `Person` data type to represent a person with a name and age.
data Person = MkPerson String Int
  deriving Show

-- Creating a `Person` named Fred who is 90 years old.
fred :: Person
fred = MkPerson "Fred" 90

-- Getting the name of a `Person`.
getName :: Person -> String
getName (MkPerson name _) = name

-- Getting the age of a `Person`.
getAge :: Person -> Int
getAge (MkPerson _ age) = age

-- Setting the name of a `Person`.
setName :: String -> Person -> Person
setName name p = MkPerson name (getAge p)

-- Setting the age of a `Person`.
setAge :: Int -> Person -> Person
setAge age p = MkPerson (getName p) age

------------------------------------------------------------------------------
-- Ex 5

-- Defining the `Position` data type to represent a position with x and y coordinates.
data Position = MkPosition { x :: Int, y :: Int }
  deriving Show

-- Creating a `Position` value with x and y both set to 0.
origin :: Position
origin = MkPosition 0 0

-- Returning the x coordinate of a `Position`.
getX :: Position -> Int
getX = x

-- Returning the y coordinate of a `Position`.
getY :: Position -> Int
getY = y

-- Increasing the y coordinate of a `Position` by one.
up :: Position -> Position
up p = p { y = y p + 1 }

-- Increasing the x coordinate of a `Position` by one.
right :: Position -> Position
right p = p { x = x p + 1 }

------------------------------------------------------------------------------
-- Ex 6

-- Defining the `Student` data type to represent different stages of a student.
data Student = Freshman | NthYear Int | Graduated
  deriving (Show, Eq)

-- Transitioning the student to the next academic year.
study :: Student -> Student
study Freshman = NthYear 1
study (NthYear 7) = Graduated
study (NthYear n) = NthYear (n + 1)
study Graduated = Graduated

------------------------------------------------------------------------------
-- Ex 7

-- Defining the `UpDown` data type to represent a value that can either be increasing or decreasing.
data UpDown = Increasing Int | Decreasing Int
  deriving Show

-- Creating an `UpDown` value starting from zero.
zero :: UpDown
zero = Increasing 0

-- Getting the current value of an `UpDown`.
get :: UpDown -> Int
get (Increasing n) = n
get (Decreasing n) = n

-- Increasing or decreasing the value of an `UpDown`.
tick :: UpDown -> UpDown
tick (Increasing n) = Increasing (n + 1)
tick (Decreasing n) = Decreasing (n - 1)

-- Toggling the state of an `UpDown` between increasing and decreasing.
toggle :: UpDown -> UpDown
toggle (Increasing n) = Decreasing n
toggle (Decreasing n) = Increasing n

------------------------------------------------------------------------------
-- Ex 8

-- Defining the `Color` data type to represent different colors and their combinations.
data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

-- Converting a `Color` to its RGB representation.
rgb :: Color -> [Double]
rgb Red = [1, 0, 0]
rgb Green = [0, 1, 0]
rgb Blue = [0, 0, 1]
rgb (Mix c1 c2) = zipWith (\x y -> (x + y) / 2) (rgb c1) (rgb c2)
rgb (Invert c) = map (1 -) (rgb c)

------------------------------------------------------------------------------
-- Ex 9

-- Defining the `OneOrTwo` data type to represent either one or two values of type `a`.
data OneOrTwo a = One a | Two a a
  deriving Show

------------------------------------------------------------------------------
-- Ex 10

-- Defining the `KeyVals` data type to represent a linked list of key-value pairs.
data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

-- Converting a `KeyVals` to a list of key-value pairs.
toList :: KeyVals k v -> [(k, v)]
toList Empty = []
toList (Pair k v rest) = (k, v) : toList rest

-- Creating a `KeyVals` from a list of key-value pairs.
fromList :: [(k, v)] -> KeyVals k v
fromList [] = Empty
fromList ((k, v) : kvs) = Pair k v (fromList kvs)

------------------------------------------------------------------------------
-- Ex 11

-- Defining the `Nat` data type to represent natural numbers.
data Nat = Zero | PlusOne Nat
  deriving (Show, Eq)

-- Converting a `Nat` to an `Int`.
fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne n) = 1 + fromNat n

-- Creating a `Nat` from an `Int`.
toNat :: Int -> Maybe Nat
toNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = fmap PlusOne (toNat (n - 1))

------------------------------------------------------------------------------
-- Ex 12

-- Defining the `Bin` data type to represent binary numbers.
data Bin = End | O Bin | I Bin
  deriving (Show, Eq)

-- Incrementing a binary number.
inc :: Bin -> Bin
inc End = I End
inc (O b) = I b
inc (I b) = O (inc b)

-- Pretty printing a binary number as a string.
prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O b) = prettyPrint b ++ "0"
prettyPrint (I b) = prettyPrint b ++ "1"

-- Converting a `Bin` to an `Int`.
fromBin :: Bin -> Int
fromBin End = 0
fromBin (O b) = 2 * fromBin b
fromBin (I b) = 1 + 2 * fromBin b

-- Creating a `Bin` from an `Int`.
toBin :: Int -> Bin
toBin 0 = O End
toBin n
  | n < 0 = error "Negative number"
  | otherwise = toBinHelper n

-- Helper function for `toBin`.
toBinHelper :: Int -> Bin
toBinHelper 0 = End
toBinHelper n
  | n `mod` 2 == 0 = O (toBinHelper (n `div` 2))
  | otherwise = I (toBinHelper (n `div` 2))
