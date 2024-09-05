module Set15 where

import Mooc.Todo
import Examples.Validation

import Control.Applicative
import Data.Char
import Text.Read (readMaybe)

import Data.Maybe

------------------------------------------------------------------------------
-- Exercise 1: Summing Two Maybe Values 

-- Sums two `Maybe Int` values. If both values are `Just`, it returns their sum as `Just`.
-- If either value is `Nothing`, it returns `Nothing`.
sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes (Just x) (Just y) = Just (x + y)
sumTwoMaybes _ _               = Nothing

------------------------------------------------------------------------------
-- Exercise 2: Creating Statements by Combining Lists of Strings with Conjunctions 

-- Creates a list of statements by combining each string from the first list with each string from the second list
-- using predefined conjunctions.
statements :: [String] -> [String] -> [String]
statements xs ys = [x ++ conj ++ y | x <- xs, conj <- conjunctions, y <- ys]
  where
    conjunctions = [" is ", " is not "]

------------------------------------------------------------------------------
-- Exercise 3: Performing Calculations Based on an Operation and a Value 

-- Applies a given operation to a value if the operation is valid. Returns the result as `Maybe Int`.
calculator :: String -> String -> Maybe Int
calculator op val = do
  f <- parseOp op
  x <- readMaybe val
  return (f x)

-- Parses a string representing an operation into a function.
parseOp :: Num a => String -> Maybe (a -> a)
parseOp "double" = Just (2*)
parseOp "negate" = Just negate
parseOp _        = Nothing

------------------------------------------------------------------------------
-- Exercise 4: Validating Division by Zero

-- Checks if division by zero is attempted. Returns the result of the division if valid, otherwise an error message.
validateDiv :: Int -> Int -> Validation Int
validateDiv x y = check (y /= 0) "Division by zero!" (x `div` y)

------------------------------------------------------------------------------
-- Exercise 5: Validating an Address

-- Represents an address with a street name, street number, and postcode.
data Address = Address String String String
  deriving (Show, Eq)

-- Validates the components of an address and constructs an `Address` if all components are valid.
validateAddress :: String -> String -> String -> Validation Address
validateAddress streetName streetNumber postCode =
  Address <$> checkStreetName streetName <*> checkStreetNum streetNumber <*> checkPostcode postCode

-- Checks if the street number is valid.
checkStreetNum :: String -> Validation String
checkStreetNum s = check (all isDigit s) "Invalid street number" s

-- Checks if the street name is valid.
checkStreetName :: String -> Validation String
checkStreetName s = check (length s <= 20) "Invalid street name" s

-- Checks if the postcode is valid.
checkPostcode :: String -> Validation String
checkPostcode s = check (all isDigit s && length s == 5) "Invalid postcode" s

------------------------------------------------------------------------------
-- Exercise 6: Creating a List of Two Person Objects

-- Represents a person with a name, age, and employment status.
data Person = Person String Int Bool
  deriving (Show, Eq)

-- Constructs a list of two `Person` objects from given applicative functors.
twoPersons :: Applicative f =>
  f String -> f Int -> f Bool -> f String -> f Int -> f Bool
  -> f [Person]
twoPersons name1 age1 employed1 name2 age2 employed2 =
  (:) <$> makePerson name1 age1 employed1 <*> ((:[]) <$> makePerson name2 age2 employed2)
  where
    makePerson = liftA3 Person

------------------------------------------------------------------------------
-- Exercise 7: Validating a String as Either a Bool or an Int

-- Validates a string as either a `Bool` or an `Int`. Returns an appropriate `Validation` result.
boolOrInt :: String -> Validation (Either Bool Int)
boolOrInt s = checkBool s <|> checkInt s
  where 
    checkBool s = check (isBool s) "Not a Bool" (Left (fromJust $ parseBool s))
    checkInt s = check (isInt s) "Not an Int" (Right (fromJust $ parseInt s))

-- Checks if a string represents a boolean value.
isBool :: String -> Bool
isBool "True"  = True
isBool "False" = True
isBool _       = False

-- Checks if a string represents an integer value.
isInt :: String -> Bool
isInt s = maybe False (const True) (parseInt s)

-- Parses a string into a `Bool`.
parseBool :: String -> Maybe Bool
parseBool s = readMaybe s

-- Parses a string into an `Int`.
parseInt :: String -> Maybe Int
parseInt s = readMaybe s

------------------------------------------------------------------------------
-- Exercise 8: Normalizing a Phone Number 

-- Normalizes a phone number by removing spaces and validating its length and characters.
normalizePhone :: String -> Validation String
normalizePhone s = (checkLen t) *> (allDigits t)
  where 
    t = filter (/=' ') s
    checkLen x = check (length x <= 10) "Too long" x
    allDigits x = traverse checkDigit x
      where 
        checkDigit a = check (isDigit a) ("Invalid character: " ++ [a]) a

------------------------------------------------------------------------------
-- Exercise 9: Parsing an Expression and Validating Its Components

-- Represents an argument in an expression, which can be either a number or a variable.
data Arg = Number Int | Variable Char
  deriving (Show, Eq)

-- Represents an expression with two arguments and an operator.
data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

-- Parses a string into an `Expression` if it is valid. Returns a `Validation` result.
parseExpression :: String -> Validation Expression
parseExpression s
  | isExpr s   = getExpr <$> (numOrVar a1) <*> (checkOp op) <*> (numOrVar a2)
  | otherwise  = invalid ("Invalid expression: " ++ s)
    where 
      [a1,op,a2] = words s

-- Constructs an `Expression` based on the operator and arguments.
getExpr :: String -> String -> String -> Expression
getExpr x op y = case op of
    "+" -> Plus (getArg x) (getArg y)
    "-" -> Minus (getArg x) (getArg y)
    _   -> error "Unsupported operator"

-- Converts a string into an `Arg`.
getArg :: String -> Arg
getArg s
  | isNum s  = Number (fromJust $ parseInt s)
  | isVar s  = Variable (head s)

-- Validates if a string is a number or a variable.
numOrVar :: String -> Validation String
numOrVar x = (checkNum x) <|> (checkVar x)

-- Checks if a string is a valid number.
checkNum x = check (isNum x) ("Invalid number: " ++ x) x

-- Checks if a string is a valid variable.
checkVar v = check (isVar v) ("Invalid variable: " ++ v) v

-- Checks if a string represents a valid operator.
checkOp op = check (isOp op) ("Unknown operator: " ++ op) op

-- Checks if a string represents a valid expression.
checkExpr :: String -> Validation String
checkExpr s = check (isExpr s) ("Invalid expression: " ++ s) s

-- Checks if a string represents a valid number.
isNum :: String -> Bool
isNum = all isDigit

-- Checks if a string represents a valid variable.
isVar :: String -> Bool
isVar [c] = isAlpha c
isVar _   = False

-- Checks if a string is a valid expression.
isExpr :: String -> Bool
isExpr s = case words s of
  [_]         -> False
  [_, _, _]   -> True
  _           -> False

-- Checks if a string is a valid operator.
isOp :: String -> Bool
isOp "+" = True
isOp "-" = True
isOp _   = False

------------------------------------------------------------------------------
-- Exercise 10: Implementing Functor and Applicative Instances for `Priced`

-- Represents a value with an associated price.
data Priced a = Priced Int a
  deriving (Show, Eq)

-- Implementing the `Functor` instance for `Priced`.
instance Functor Priced where
  fmap f pricedValue = 
    case pricedValue of
        Priced price a -> Priced price (f a)

-- Implementing the `Applicative` instance for `Priced`.
instance Applicative Priced where
  pure x = Priced 0 x

  -- Implementing `liftA2` for the `Applicative` instance.
  liftA2 f pa1 pa2 = 
    case (pa1, pa2) of
      (Priced p1 a1, Priced p2 a2) -> Priced (p1 + p2) (f a1 a2)

------------------------------------------------------------------------------
-- Exercise 11: Creating a Custom Applicative Class

-- A custom `Applicative` class with `myPure` and `myLiftA2` methods.
class MyApplicative f where
  myPure :: a -> f a
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

-- Providing instances for the `MyApplicative` class for `Maybe` and lists.
instance MyApplicative Maybe where
  myPure = pure
  myLiftA2 = liftA2

instance MyApplicative [] where
  myPure = pure
  myLiftA2 = liftA2

-- Custom operator `<#>` for applying a function in an applicative context.
(<#>) :: MyApplicative f => f (a -> b) -> f a -> f b
f <#> x = myLiftA2 ($) f x

------------------------------------------------------------------------------
-- Exercise 12: Implementing `fmap` Using the Custom Applicative

-- Defines `myFmap` using the custom `Applicative` class.
myFmap :: MyApplicative f => (a -> b) -> f a -> f b
myFmap f x = (myPure f) <#> x

------------------------------------------------------------------------------
-- Exercise 13: Trying All Functions from a List Until One Succeeds

-- Applies a function to each element of a list, returning the result of the first successful application.
tryAll :: Alternative f => (a -> f b) -> [a] -> f b
tryAll f = foldr (\x acc -> f x <|> acc) empty

------------------------------------------------------------------------------
-- Exercise 14: Implementing `Functor` for `Both f g`

-- Represents a value wrapped in two functors.
newtype Both f g a = Both (f (g a))
  deriving Show

-- Implementing the `Functor` instance for `Both f g`.
instance (Functor f, Functor g) => Functor (Both f g) where
  fmap t (Both f) = Both ((fmap . fmap) t f)

------------------------------------------------------------------------------
-- Exercise 15: Implementing `Applicative` for `Both f g`

-- Implementing the `Applicative` instance for `Both f g`.
instance (Applicative f, Applicative g) => Applicative (Both f g) where
  pure x = Both (pure (pure x))

  -- Implementing `liftA2` for the `Applicative` instance.
  liftA2 f (Both a) (Both b) = Both (liftA2 (liftA2 f) a b)
