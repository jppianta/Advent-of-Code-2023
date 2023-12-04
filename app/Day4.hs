module Day4 where

import Utils (splitOn)
import Data.Set hiding (drop, map)

type CardBoard = [(WinningCard, ElfNumbers)]
type WinningCard = (WinningNumbers, Int)

type WinningNumbers = Set Int
type ElfNumbers = Set Int

-- Part 1
sumOfPoints :: [(WinningNumbers, ElfNumbers)] -> Int
sumOfPoints cards = sum (map (uncurry pointsOfElfNumbers) cards)

pointsOfElfNumbers :: ElfNumbers -> WinningNumbers -> Int
pointsOfElfNumbers elfNumbers winningNumbers
  | n == 0 = 0
  | n > 0 = 2 ^ (n - 1)
  | otherwise = 0
  where n = numberOfIntersections winningNumbers elfNumbers

numberOfIntersections :: WinningNumbers -> ElfNumbers -> Int
numberOfIntersections winningNumbers elfNumbers = size (intersection winningNumbers elfNumbers)
---

-- Part 2
addNextCopies :: CardBoard -> Int -> Int -> CardBoard
addNextCopies [] _ _ = []
addNextCopies b 0 _ = b
addNextCopies (((card, count), e):xs) c n = ((card, count + n), e) : addNextCopies xs (c - 1) n

addNextCopiesOfCard :: CardBoard -> (WinningCard, ElfNumbers) -> CardBoard
addNextCopiesOfCard b ((cards, count), elfNumbers) = addNextCopies b (numberOfIntersections cards elfNumbers) count

runBoard :: CardBoard -> CardBoard
runBoard (card:xs) = card : runBoard (addNextCopiesOfCard xs card)
runBoard [] = []

sumBoard :: CardBoard -> Int
sumBoard b = sum (map (snd . fst) (runBoard b))
---

-- Parsers
parseCards :: [[Int]] -> (WinningNumbers, ElfNumbers)
parseCards (x:y:_) = (fromList x, fromList y)
parseCards [x] = (fromList x, empty)
parseCards [] = (empty, empty)

parseLine :: String -> (WinningNumbers, ElfNumbers)
parseLine s = parseCards (map parseNumbers (splitOn '|' (drop 9 s)))

parseCardsPart2 :: [[Int]] -> (WinningCard, ElfNumbers)
parseCardsPart2 (x:y:_) = ((fromList x, 1), fromList y)
parseCardsPart2 [x] = ((fromList x, 1), empty)
parseCardsPart2 [] = ((empty, 1), empty)

parseLinePart2 :: String -> (WinningCard, ElfNumbers)
parseLinePart2 s = parseCardsPart2 (map parseNumbers (splitOn '|' (drop 9 s)))

createCardBoard :: [String] -> CardBoard
createCardBoard = map parseLinePart2

parseNumbers :: String -> [Int]
parseNumbers s = map read (words s)
---

executeDay4Part1 :: IO ()
executeDay4Part1 = do
  contents <- readFile "inputFiles/Day4.txt"
  let file = lines contents
  let cards = map parseLine file
  print (sumOfPoints cards)

executeDay4Part2 :: IO ()
executeDay4Part2 = do
  contents <- readFile "inputFiles/Day4.txt"
  let file = lines contents
  let cardBoard = createCardBoard file
  print (sumBoard cardBoard)
