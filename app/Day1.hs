module Day1 where

import Data.Char (isDigit)
import Data.List

-- | Checks for the first occurence of a number, either a digit or a written digit
stripFirstNumber :: String -> String
stripFirstNumber "" = ""
stripFirstNumber (x:xs)
  | isDigit x = [x]
  | isWrittenDigit (x:xs) = writtenDigitsToDigits (stripWrittenDigit (x:xs))
  | otherwise = stripFirstNumber xs

-- | Checks for the last occurence of a number, either a digit or a written digit
stripLastNumber :: String -> String
stripLastNumber "" = ""
stripLastNumber x
  | isDigit (last x) = [last x]
  | isWrittenDigitSuffix x = writtenDigitsToDigits (stripWrittenDigitSuffix x)
  | otherwise = stripLastNumber (last x : init x)

-- | List of written digits
writtenDigits :: [String]
writtenDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- | Converts written version to digit string
writtenDigitsToDigits :: Maybe String -> String
writtenDigitsToDigits x
  | x == Just "one" = "1"
  | x == Just "two" = "2"
  | x == Just "three" = "3"
  | x == Just "four" = "4"
  | x == Just "five" = "5"
  | x == Just "six" = "6"
  | x == Just "seven" = "7"
  | x == Just "eight" = "8"
  | x == Just "nine" = "9"
  | otherwise = ""

-- | Verifies if string starts with any of the written version of the digits
isWrittenDigit :: String -> Bool
isWrittenDigit x = any (`isPrefixOf` x) writtenDigits

-- | Returns written digit if finds one, otherwise returns Nothing
stripWrittenDigit :: String -> Maybe String
stripWrittenDigit "" = Nothing
stripWrittenDigit x
  | isWrittenDigit x = find (`isPrefixOf` x) writtenDigits
  | otherwise = Nothing

-- | Verifies if string ends with any of the written version of the digits
isWrittenDigitSuffix :: String -> Bool
isWrittenDigitSuffix x = any (`isSuffixOf` x) writtenDigits

-- | Returns written digit if finds one, otherwise returns Nothing
stripWrittenDigitSuffix :: String -> Maybe String
stripWrittenDigitSuffix "" = Nothing
stripWrittenDigitSuffix x
  | isWrittenDigitSuffix x = find (`isSuffixOf` x) writtenDigits
  | otherwise = Nothing

-- | Combines first and last numbers found and converts to number
parseLine :: String -> Int
parseLine x = read (stripFirstNumber x ++ stripLastNumber x)

-- | Parses all lines and sum to get the result
getResult :: String -> Int
getResult contents = sum (map parseLine (lines contents))

-- | Reads the input file and gets the result
executeDay1 :: IO ()
executeDay1 = do
  contents <- readFile "inputFiles/Day1.txt"
  print (getResult contents)