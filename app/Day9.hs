module Day9 where

-- Part 1
lastNumber :: [[Int]] -> Int
lastNumber [] = 0
lastNumber [x] = head x
lastNumber (x : y : xs) = head x + lastNumber (y : xs)

getDiffListReverse :: [Int] -> [[Int]]
getDiffListReverse x = reverse (reverse x : diffListReverse x)

diffListReverse :: [Int] -> [[Int]]
diffListReverse x
  | allZeros next = [reverse next]
  | otherwise = reverse next : diffListReverse next
  where
    next = nextDiffList x

nextDiffList :: [Int] -> [Int]
nextDiffList [] = []
nextDiffList (x : y : xs) = y - x : nextDiffList (y : xs)
nextDiffList [_] = []

---

-- Part 2
firstNumber :: [[Int]] -> Int
firstNumber [] = 0
firstNumber [x] = head x
firstNumber (x : y : xs) = head x - firstNumber (y : xs)

getDiffList :: [Int] -> [[Int]]
getDiffList x = x : diffList x

diffList :: [Int] -> [[Int]]
diffList x
  | allZeros next = [next]
  | otherwise = next : diffList next
  where
    next = nextDiffList x

---

allZeros :: [Int] -> Bool
allZeros = all (== 0)

-- Parser
parseLine :: [String] -> [Int]
parseLine = map read

---

executeDay9Part1 :: IO ()
executeDay9Part1 = do
  contents <- readFile "inputFiles/Day9.txt"
  let content = lines contents
  let file = map words content
  let histories = map parseLine file
  let histDiffList = map getDiffListReverse histories
  print (sum (map lastNumber histDiffList))

executeDay9Part2 :: IO ()
executeDay9Part2 = do
  contents <- readFile "inputFiles/Day9.txt"
  let content = lines contents
  let file = map words content
  let histories = map parseLine file
  let histDiffList = map getDiffList histories
  print (sum (map firstNumber histDiffList))
