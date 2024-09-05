module Set6 where

import Mooc.Todo
import Data.Char (toLower)

------------------------------------------------------------------------------
-- Ex 1

-- Defining a data type 'Country' with three possible values: Finland, Switzerland, and Norway.
data Country = Finland | Switzerland | Norway
  deriving Show

-- Making 'Country' an instance of the 'Eq' typeclass by implementing equality checking.
instance Eq Country where
  Finland == Finland = True
  Switzerland == Switzerland = True
  Norway == Norway = True
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 2

-- Making 'Country' an instance of the 'Ord' typeclass by defining comparison functions.
instance Ord Country where
  compare Finland Finland = EQ
  compare Finland _ = LT
  compare Norway Finland = GT
  compare Norway Norway = EQ
  compare Norway Switzerland = LT
  compare Switzerland Switzerland = EQ
  compare Switzerland _ = GT

  -- Implementing the less-than-or-equal comparison function.
  Finland <= Norway = True
  Norway <= Switzerland = True
  Finland <= Switzerland = True
  x <= y = compare x y /= GT

  -- Defining 'min' and 'max' functions for 'Country'.
  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

------------------------------------------------------------------------------
-- Ex 3

-- Defining a data type 'Name' with a single String value.
data Name = Name String
  deriving Show

-- Making 'Name' an instance of the 'Eq' typeclass by implementing case-insensitive comparison.
instance Eq Name where
  (Name n1) == (Name n2) = map toLower n1 == map toLower n2

------------------------------------------------------------------------------
-- Ex 4

-- Defining a data type 'List' which can be either 'Empty' or a node containing an element and the rest of the list.
data List a = Empty | LNode a (List a)
  deriving Show

-- Making 'List' an instance of the 'Eq' typeclass by comparing elements recursively.
instance Eq a => Eq (List a) where
  Empty == Empty = True
  (LNode x xs) == (LNode y ys) = x == y && xs == ys
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 5

-- Defining two data types: 'Egg' (with ChickenEgg and ChocolateEgg) and 'Milk' (with an amount in litres).
data Egg = ChickenEgg | ChocolateEgg
  deriving Show
data Milk = Milk Int -- Amount in litres
  deriving Show

-- Defining a 'Price' typeclass with a 'price' function.
class Price a where
  price :: a -> Int

-- Making 'Egg' an instance of 'Price' by assigning specific prices to each type of egg.
instance Price Egg where
  price ChickenEgg = 20
  price ChocolateEgg = 30

-- Making 'Milk' an instance of 'Price' by calculating the price based on the number of litres.
instance Price Milk where
  price (Milk litres) = 15 * litres

------------------------------------------------------------------------------
-- Ex 6

-- Making 'Maybe' an instance of 'Price' by defining how to get the price from a 'Maybe' type.
instance Price a => Price (Maybe a) where
  price (Just x) = price x
  price Nothing = 0

-- Making lists an instance of 'Price' by summing up the prices of all elements in the list.
instance Price a => Price [a] where
  price = sum . map price

------------------------------------------------------------------------------
-- Ex 7

-- Defining a data type 'Number' which can be either 'Finite' (with an integer value) or 'Infinite'.
data Number = Finite Integer | Infinite
  deriving (Show, Eq)

-- Making 'Number' an instance of the 'Ord' typeclass by defining the comparison rules for finite and infinite numbers.
instance Ord Number where
  Finite x <= Finite y = x <= y
  Finite _ <= Infinite = True
  Infinite <= Finite _ = False
  Infinite <= Infinite = True

------------------------------------------------------------------------------
-- Ex 8

-- Defining a data type 'RationalNumber' with two integer values representing the numerator and denominator.
data RationalNumber = RationalNumber Integer Integer
  deriving Show

-- Making 'RationalNumber' an instance of the 'Eq' typeclass by implementing equality checking through cross multiplication.
instance Eq RationalNumber where
  (RationalNumber a b) == (RationalNumber c d) = a * d == b * c

------------------------------------------------------------------------------
-- Ex 9

-- Defining a function 'simplify' to reduce a 'RationalNumber' by dividing both the numerator and denominator by their greatest common divisor (gcd).
simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber a b) = RationalNumber (a `div` gcd_ab) (b `div` gcd_ab)
  where gcd_ab = gcd a b

------------------------------------------------------------------------------
-- Ex 10

-- Making 'RationalNumber' an instance of the 'Num' typeclass by defining arithmetic operations.
instance Num RationalNumber where
  (RationalNumber a b) + (RationalNumber c d) = simplify (RationalNumber (a*d + c*b) (b*d))
  (RationalNumber a b) * (RationalNumber c d) = simplify (RationalNumber (a*c) (b*d))
  abs (RationalNumber a b) = RationalNumber (abs a) (abs b)
  signum (RationalNumber a b) = RationalNumber (signum a * signum b) 1
  fromInteger x = RationalNumber x 1
  negate (RationalNumber a b) = RationalNumber (negate a) b

------------------------------------------------------------------------------
-- Ex 11

-- Defining an 'Addable' typeclass with 'zero' and 'add' functions for addition.
class Addable a where
  zero :: a
  add :: a -> a -> a

-- Making 'Integer' an instance of 'Addable' by defining 'zero' and 'add' functions.
instance Addable Integer where
  zero = 0
  add = (+)

-- Making lists an instance of 'Addable' by defining 'zero' as an empty list and 'add' as list concatenation.
instance Addable [a] where
  zero = []
  add = (++)

------------------------------------------------------------------------------
-- Ex 12

-- Defining two data types: 'Color' (with Red, Green, and Blue) and 'Suit' (with Club, Spade, Diamond, and Heart).
data Color = Red | Green | Blue
  deriving (Show, Eq)
data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq)

-- Defining a 'Cycle' typeclass with 'step' and 'stepMany' functions for cycling through values.
class Cycle a where
  step :: a -> a
  stepMany :: Int -> a -> a
  stepMany 0 a = a
  stepMany 1 a = step a
  stepMany n a = stepMany (n-1) (step a)

-- Making 'Color' an instance of 'Cycle' by defining how to step through the colors.
instance Cycle Color where
  step Red = Green
  step Green = Blue
  step Blue = Red

-- Making 'Suit' an instance of 'Cycle' by defining how to step through the suits.
instance Cycle Suit where
  step Club    = Spade
  step Spade   = Diamond
  step Diamond = Heart
  step Heart   = Club
