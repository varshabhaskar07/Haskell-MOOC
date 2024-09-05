module Set14a where

import Mooc.Todo
import Data.Bits
import Data.Char
import Data.Text.Encoding
import Data.Word
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

------------------------------------------------------------------------------
-- Ex 1: Generate a greeting with a name

-- Generate a greeting text based on the length of the name. 
-- If the name is longer than 15 characters, truncate it and append "..." at the end.
greetText :: T.Text -> T.Text
greetText name
    | T.length name <= 15 = T.pack ("Hello, " ++ T.unpack name ++ "!")
    | otherwise = T.pack ("Hello, " ++ T.unpack (T.take 15 name) ++ "...!")

------------------------------------------------------------------------------
-- Ex 2: Transform text to shouting format

-- Convert the text to shouting format by capitalizing every alternate word.
shout :: T.Text -> T.Text
shout = T.unwords . zipWith (\i word -> if i `mod` 2 == 0 then T.toUpper word else word) [0..] . T.words

------------------------------------------------------------------------------
-- Ex 3: Find the length of the longest repeating character sequence

-- Find the length of the longest sequence of repeating characters in the text.
longestRepeat :: T.Text -> Int
longestRepeat text = longestRepeat' (head "", 0) text

-- Helper function to recursively find the length of the longest repeating character sequence.
longestRepeat' :: (Char, Int) -> T.Text -> Int
longestRepeat' (c, i) t = case T.uncons t of
                            Nothing       -> i
                            Just (x,rest) -> if len >= i
                                             then longestRepeat' (x, len) rest
                                             else longestRepeat' (c, i) rest
                                                where seq = T.takeWhile (== x) t
                                                      len = T.length seq

------------------------------------------------------------------------------
-- Ex 4: Convert a portion of lazy text to strict text

-- Take a portion of the lazy text (up to `n` characters) and convert it to strict text.
takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n = T.take (fromIntegral n) . TL.toStrict . TL.take n

------------------------------------------------------------------------------
-- Ex 5: Calculate the byte range

-- Calculate the range of bytes in a `ByteString` by finding the difference between the maximum and minimum byte values.
byteRange :: B.ByteString -> Word8
byteRange bs
    | B.null bs = 0
    | otherwise = B.maximum bs - B.minimum bs

------------------------------------------------------------------------------
-- Ex 6: Compute XOR checksum

-- Compute the XOR checksum of a `ByteString` by folding the XOR operation over all byte values.
xorChecksum :: B.ByteString -> Word8
xorChecksum = B.foldl' xor 0

------------------------------------------------------------------------------
-- Ex 7: Count UTF-8 characters

-- Count the number of UTF-8 characters in a `ByteString`. Return `Nothing` if decoding fails.
countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars bs = case decodeUtf8' bs of
    Left _  -> Nothing
    Right t -> Just (T.length t)

------------------------------------------------------------------------------
-- Ex 8: Generate a ping-pong pattern

-- Create a `ByteString` with a ping-pong pattern by cycling through the byte string and its reverse.
pingpong :: B.ByteString -> BL.ByteString
pingpong bs = BL.cycle (BL.fromStrict bs <> BL.reverse (BL.fromStrict bs))
