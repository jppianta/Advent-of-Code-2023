module Day3 where

import Data.Char (isDigit)
import Data.List (find)
import qualified Data.HashMap as HM

type Coord = (Int, Int)

type ValueCoord = (Char, Coord)

type EngineSchematic = [ValueCoord]

-- Part 2
type PartNumberPair = (Int, [ValueCoord])

executePart2 :: EngineSchematic -> Int
executePart2 eng = sumGearRatios (parseGears eng)

parseGears :: EngineSchematic -> HM.Map (Int, Int) [Int]
parseGears engine = parseGearValues (parsePartNumberPairs (extractNumbers engine) engine)

sumGearRatios :: HM.Map (Int, Int) [Int] -> Int
sumGearRatios m = HM.fold (\vals acc -> acc + gearRatio vals) 0 m 

gearRatio :: [Int] -> Int
gearRatio [] = 0
gearRatio (_:[]) = 0
gearRatio (x:y:_) = x * y

parseGearValues :: [PartNumberPair] -> HM.Map (Int, Int) [Int]
parseGearValues = foldr parseGearValue HM.empty

parseGearValue :: PartNumberPair -> HM.Map (Int, Int) [Int] -> HM.Map (Int, Int) [Int]
parseGearValue (_, []) m = m
parseGearValue (v, ((_, coord):xs)) m = HM.insert coord (v : (emptyIfNothing (HM.lookup coord m))) (parseGearValue (v, xs) m)

emptyIfNothing :: Maybe [a] -> [a]
emptyIfNothing Nothing = []
emptyIfNothing (Just l) = l

parsePartNumberPairs :: [[ValueCoord]] -> EngineSchematic -> [PartNumberPair]
parsePartNumberPairs values eng = filter (\ (_, l) -> not (null l)) (map (`createPartNumberPair` eng) values)

createPartNumberPair :: [ValueCoord] -> EngineSchematic -> PartNumberPair
createPartNumberPair pn eng = (parsePartNumber pn eng, concatMap ((`findAdjencentGears` eng) . snd) pn)

findAdjencentGears :: Coord -> EngineSchematic -> [ValueCoord]
findAdjencentGears c e =  filter (\ (v, _) -> v == '*') (adjacentValueCoords c e)

adjacentValueCoords :: Coord -> EngineSchematic -> [ValueCoord]
adjacentValueCoords c e = map (`valueCoordFromCoord` e) (adjacentCoords c)

valueCoordFromCoord :: Coord -> EngineSchematic -> ValueCoord
valueCoordFromCoord c e = extractValueCoord (maybeValueFromCoord c e)

extractValueCoord :: Maybe ValueCoord -> ValueCoord
extractValueCoord Nothing = ('.', (-1, -1))
extractValueCoord (Just v) = v

-- Part 1
sumPartNumbers :: EngineSchematic -> Int
sumPartNumbers engine = sum (parsePartNumbers (extractNumbers engine) engine)

parsePartNumbers :: [[ValueCoord]] -> EngineSchematic -> [Int]
parsePartNumbers values eng = map (`parsePartNumber` eng) values

parsePartNumber :: [ValueCoord] -> EngineSchematic -> Int
parsePartNumber value eng
  | isPartNumber value eng = numberFromValueCords value
  | otherwise = 0

numberFromValueCords :: [ValueCoord] -> Int
numberFromValueCords v = read (map fst v)

isPartNumber :: [ValueCoord] -> EngineSchematic -> Bool
isPartNumber n eng = any (`hasAdjecentSymbol` eng) n

extractNumbers :: EngineSchematic -> [[ValueCoord]]
extractNumbers [] = []
extractNumbers ((v, coord) : xs)
  | isDigit v = number : extractNumbers (drop (length number) xs)
  | otherwise = extractNumbers xs
  where
    number = extractValueNumber ((v, coord) : xs)

extractValueNumber :: EngineSchematic -> [ValueCoord]
extractValueNumber [] = []
extractValueNumber ((v, coord) : xs)
  | isPartOfNumber (v, coord) ((v, coord) : xs) = (v, coord) : extractValueNumber xs
  | isDigit v = [(v, coord)]
  | otherwise = []

isPartOfNumber :: ValueCoord -> EngineSchematic -> Bool
isPartOfNumber (_, coord) eng = isDigit (valueFromCoord (addCoords coord (0, 1)) eng)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

hasAdjecentSymbol :: ValueCoord -> EngineSchematic -> Bool
hasAdjecentSymbol v eng = containsAdjecentSymbol (map (`valueFromCoord` eng) (getAdjacentCoords v))

containsAdjecentSymbol :: [Char] -> Bool
containsAdjecentSymbol [] = False
containsAdjecentSymbol (x : xs)
  | isSymbol x = True
  | otherwise = containsAdjecentSymbol xs

adjacents :: [Coord]
adjacents = [(-1, 0), (-1, -1), (-1, 1), (0, -1), (0, 1), (1, 0), (1, -1), (1, 1)]

getAdjacentCoords :: ValueCoord -> [Coord]
getAdjacentCoords (v, coord) = map (addCoords coord) adjacents

adjacentCoords :: Coord -> [Coord]
adjacentCoords c = map (addCoords c) adjacents

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

valueFromCoord :: Coord -> EngineSchematic -> Char
valueFromCoord c e = extractValueFromValueCoord (maybeValueFromCoord c e)

maybeValueFromCoord :: Coord -> EngineSchematic -> Maybe ValueCoord
maybeValueFromCoord coord = find (\(v, c) -> c == coord)

extractValueFromValueCoord :: Maybe ValueCoord -> Char
extractValueFromValueCoord Nothing = '.'
extractValueFromValueCoord (Just v) = fst v

---
-- Parsers
getCoordsFromLine :: Int -> String -> [Coord]
getCoordsFromLine idx line = map (\x -> (idx, x)) (indexesFromZero line)

parseLine :: Int -> String -> [ValueCoord]
parseLine idx line = zip line (getCoordsFromLine idx line)

parseFile :: [String] -> EngineSchematic
parseFile file = concat (zipWith parseLine (indexesFromZero file) file)
---

-- Helpers
indexes :: Int -> [a] -> [Int]
indexes n [] = []
indexes n (x : xs) = n : indexes (n + 1) xs

indexesFromZero :: [a] -> [Int]
indexesFromZero = indexes 0
---

executeDay3Part1 :: IO ()
executeDay3Part1 = do
  contents <- readFile "inputFiles/Day3.txt"
  let file = lines contents
  let engine = parseFile file
  print (sumPartNumbers engine)

executeDay3Part2 :: IO ()
executeDay3Part2 = do
  contents <- readFile "inputFiles/Day3.txt"
  let file = lines contents
  let engine = parseFile file
  print (executePart2 engine)
