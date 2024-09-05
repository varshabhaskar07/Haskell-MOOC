module Set16b where

import Mooc.Todo
import Examples.Phantom

import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Exercise 1: Defining a Currency Type for GBP and Creating an Example Amount

-- Declaring a data type for GBP (British Pound Sterling). This type serves as a marker for the currency.
data GBP

-- Creating an example amount of 3 GBP using the Money type constructor for GBP currency.
pounds :: Money GBP
pounds = Money 3

------------------------------------------------------------------------------
-- Exercise 2: Converting Rates and Composing Them

-- Specifying the conversion rate from USD (United States Dollar) to CHF (Swiss Franc).
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

-- Composing two conversion rates to obtain a combined conversion rate.
-- For example, if you have a rate from USD to EUR and a rate from EUR to CHF,
-- this function will provide a rate from USD to CHF by multiplying the two rates.
composeRates :: Rate a b -> Rate b c -> Rate a c
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

------------------------------------------------------------------------------
-- Exercise 3: Working with Name Types and Transformations

-- Defining placeholder data types for different parts of a name.
data First
data Last
data Full

-- Creating a newtype for representing names with a parameter for different name parts.
-- The `Name` type encapsulates a `String` for various name parts.
newtype Name a = Name String

-- Extracting the string representation from a `Name` value.
fromName :: Name a -> String
fromName (Name s) = s

-- Converting a string into a `First` name.
toFirst :: String -> Name First
toFirst = Name

-- Converting a string into a `Last` name.
toLast :: String -> Name Last
toLast = Name

------------------------------------------------------------------------------
-- Exercise 4: Manipulating and Combining Names

-- Capitalizing the first letter of a `Name` while preserving the rest of the string unchanged.
-- If the name is empty, it remains unchanged.
capitalize :: Name a -> Name a
capitalize (Name (x:xs)) = Name (toUpper x : xs)
capitalize (Name "") = Name ""

-- Combining a `First` name and a `Last` name to form a `Full` name.
-- The result is a `Name` of type `Full` with the format "First Last".
toFull :: Name First -> Name Last -> Name Full
toFull (Name first) (Name last) = Name (first ++ " " ++ last)

------------------------------------------------------------------------------
-- Exercise 5: Rendering Money Values in Different Currencies

-- Defining a typeclass `Render` for rendering money values in various currencies.
-- The `render` function converts a `Money` value into its string representation.
class Render currency where
  render :: Money currency -> String

-- Rendering the EUR (Euro) currency with an "e" suffix.
instance Render Examples.Phantom.EUR where
  render (Money amount) = show amount ++ "e"

-- Rendering the USD (United States Dollar) currency with a "$" prefix.
instance Render Examples.Phantom.USD where
  render (Money amount) = "$" ++ show amount

-- Rendering the CHF (Swiss Franc) currency with a "CHF" suffix.
instance Render Examples.Phantom.CHF where
  render (Money amount) = show amount ++ "CHF"
