module Day1 (executeDay1) where

import Data.Char (isDigit)
import Data.List

stripFirstNumber :: String -> String
stripFirstNumber "" = ""
stripFirstNumber (x:xs)
  | isDigit x = [x]
  | isWrittenDigit (x:xs) = writtenDigitsToDigits (stripWrittenDigit (x:xs))
  | otherwise = stripFirstNumber xs

-- Basically same as stripFirstNumber but going backwards through the string
stripLastNumber :: String -> String
stripLastNumber "" = ""
stripLastNumber x
  | isDigit (last x) = [last x]
  | isWrittenDigitSuffix x = writtenDigitsToDigits (stripWrittenDigitSuffix x)
  | otherwise = stripLastNumber (last x : init x)

writtenDigits :: [String]
writtenDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

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

isWrittenDigit :: String -> Bool
isWrittenDigit x = any (`isPrefixOf` x) writtenDigits

stripWrittenDigit :: String -> Maybe String
stripWrittenDigit "" = Nothing
stripWrittenDigit x
  | isWrittenDigit x = find (`isPrefixOf` x) writtenDigits

isWrittenDigitSuffix :: String -> Bool
isWrittenDigitSuffix x = any (`isSuffixOf` x) writtenDigits

stripWrittenDigitSuffix :: String -> Maybe String
stripWrittenDigitSuffix "" = Nothing
stripWrittenDigitSuffix x
  | isWrittenDigitSuffix x = find (`isSuffixOf` x) writtenDigits

parseLine :: String -> Int
parseLine x = read (stripFirstNumber x ++ stripLastNumber x)

getResult :: String -> Int
getResult contents = sum (map parseLine (lines contents))

executeDay1 :: IO ()
executeDay1 = do
  contents <- readFile "inputFiles/Day1.txt"
  print (getResult contents)