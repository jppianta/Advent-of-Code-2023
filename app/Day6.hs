module Day6 where

type Time = Int
type Distance = Int
type Race = (Time, Distance)

multiplyWaysToWin :: [Race] -> Int
multiplyWaysToWin = foldl (*) 1 . map waysToWin

waysToWin :: Race -> Int
waysToWin (time, distance) = length (filter (>distance) (map (distanceOnHold time) [0..time]))

distanceOnHold :: Int -> Int -> Int
distanceOnHold time holdingTime = holdingTime * (time - holdingTime)

-- Parsers
parseRaces :: [String] -> [Race];
parseRaces [t,d] = zip (map read (drop 1 (words t))) (map read (drop 1 (words d)))
parseRaces [] = []
parseRaces [_] = []
parseRaces _ = []

parseRace :: [String] -> Race
parseRace [t,d] = (parseNumber (drop 1 (words t)), parseNumber (drop 1 (words d)))
parseRace [] = (0,0)
parseRace [_] = (0,0)
parseRace _ = (0,0)

parseNumber :: [String] -> Int
parseNumber = read . foldl (++) ""
---

executeDay6Part1 :: IO ()
executeDay6Part1 = do
  contents <- readFile "inputFiles/Day6.txt"
  let file = lines contents
  let races = parseRaces file
  print (multiplyWaysToWin races)

executeDay6Part2 :: IO ()
executeDay6Part2 = do
  contents <- readFile "inputFiles/Day6.txt"
  let file = lines contents
  let race = parseRace file
  print (waysToWin race)