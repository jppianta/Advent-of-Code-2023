module Day3 where

import Prelude hiding (lookup)
import Data.Char (isDigit)
import Data.List (sort)
import Data.HashMap hiding (map, filter, null)

type Coord = (Int, Int)

type ValueCoord = (Char, Coord)

type EngineSchematic = Map Coord Char

type PartNumber = (Int, [Coord])

type GearPair = (Int, [ValueCoord])

-- Part 2
executePart2 :: EngineSchematic -> Int
executePart2 eng = sumGearRatios (parseGears eng)

parseGears :: EngineSchematic -> Map (Int, Int) [Int]
parseGears engine = parseGearValues (parseGearPairs (extractNumbers engine) engine)

sumGearRatios :: Map (Int, Int) [Int] -> Int
sumGearRatios m = fold (\vals acc -> acc + gearRatio vals) 0 m 

gearRatio :: [Int] -> Int
gearRatio [] = 0
gearRatio (_:[]) = 0
gearRatio (x:y:_) = x * y

parseGearValues :: [GearPair] -> Map (Int, Int) [Int]
parseGearValues = foldr parseGearValue empty

parseGearValue :: GearPair -> Map (Int, Int) [Int] -> Map (Int, Int) [Int]
parseGearValue (_, []) m = m
parseGearValue (v, ((_, coord):xs)) m = insert coord (v : (emptyIfNothing (lookup coord m))) (parseGearValue (v, xs) m)

emptyIfNothing :: Maybe [a] -> [a]
emptyIfNothing Nothing = []
emptyIfNothing (Just l) = l

parseGearPairs :: [PartNumber] -> EngineSchematic -> [GearPair]
parseGearPairs values eng = filter (\ (_, l) -> not (null l)) (map (`createGearPair` eng) values)

createGearPair :: PartNumber -> EngineSchematic -> GearPair
createGearPair (n, coord) eng = (parsePartNumber (n, coord) eng, concat (map (`findAdjencentGears` eng) coord))

findAdjencentGears :: Coord -> EngineSchematic -> [ValueCoord]
findAdjencentGears c e =  filter (\ (v, _) -> v == '*') (adjacentValueCoords c e)

adjacentValueCoords :: Coord -> EngineSchematic -> [ValueCoord]
adjacentValueCoords c e = map (`valueCoordFromCoord` e) (adjacentCoords c)

valueCoordFromCoord :: Coord -> EngineSchematic -> ValueCoord
valueCoordFromCoord c e = extractValueCoord c (lookup c e)

extractValueCoord :: Coord -> Maybe Char -> ValueCoord
extractValueCoord c Nothing = ('.', c)
extractValueCoord c (Just v) = (v, c)

-- -- Part 1
sumPartNumbers :: EngineSchematic -> Int
sumPartNumbers eng = sum (parsePartNumbers (extractNumbers eng) eng)

parsePartNumbers :: [PartNumber] -> EngineSchematic -> [Int]
parsePartNumbers values eng = map (`parsePartNumber` eng) values

parsePartNumber :: PartNumber -> EngineSchematic -> Int
parsePartNumber (value, coords) eng
  | isPartNumber coords eng = value
  | otherwise = 0

isPartNumber :: [Coord] -> EngineSchematic -> Bool
isPartNumber n eng = any (`hasAdjecentSymbol` eng) n

extractNumbers :: EngineSchematic -> [PartNumber]
extractNumbers eng = extractNumber (sort (keys eng)) eng

extractNumber :: [Coord] -> EngineSchematic -> [PartNumber]
extractNumber [] _ = []
extractNumber (coord:xs) eng
  | isDigit look = (number, snd pair) : extractNumber xs (removeAllCoords (snd pair) eng)
  | otherwise = extractNumber xs eng
  where
    look = valueFromCoord coord eng
    pair = extractValueNumber coord eng
    number = read (fst pair) :: Int

removeAllCoords :: [Coord] -> EngineSchematic -> EngineSchematic
removeAllCoords coords eng = foldr delete eng coords

extractValueNumber :: Coord -> EngineSchematic -> (String, [Coord])
extractValueNumber coord eng
  | isPartOfNumber coord eng = joinPartNumbers ([look], [coord]) (extractValueNumber (addCoords coord (0, 1)) eng)
  | isDigit look = ([look], [coord])
  | otherwise = ("", [])
  where look = valueFromCoord coord eng

joinPartNumbers :: (String, [Coord]) -> (String, [Coord]) -> (String, [Coord])
joinPartNumbers (s1, c1) (s2, c2) = (s1 ++ s2, c1 ++ c2)

isPartOfNumber :: Coord -> EngineSchematic -> Bool
isPartOfNumber coord eng = isDigit (valueFromCoord (addCoords coord (0, 1)) eng)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

hasAdjecentSymbol :: Coord -> EngineSchematic -> Bool
hasAdjecentSymbol c eng = containsAdjecentSymbol (map (`valueFromCoord` eng) (getAdjacentCoords c))

containsAdjecentSymbol :: [Char] -> Bool
containsAdjecentSymbol [] = False
containsAdjecentSymbol (x : xs)
  | isSymbol x = True
  | otherwise = containsAdjecentSymbol xs

adjacents :: [Coord]
adjacents = [(-1, 0), (-1, -1), (-1, 1), (0, -1), (0, 1), (1, 0), (1, -1), (1, 1)]

getAdjacentCoords :: Coord -> [Coord]
getAdjacentCoords coord = map (addCoords coord) adjacents

adjacentCoords :: Coord -> [Coord]
adjacentCoords c = map (addCoords c) adjacents

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

valueFromCoord :: Coord -> EngineSchematic -> Char
valueFromCoord c e = extractValueFromValueCoord (lookup c e)

extractValueFromValueCoord :: Maybe Char -> Char
extractValueFromValueCoord Nothing = '.'
extractValueFromValueCoord (Just v) = v

---
-- Parsers
getCoordsFromLine :: Int -> String -> [Coord]
getCoordsFromLine idx line = map (\x -> (idx, x)) (indexesFromZero line)

parseLine :: Int -> String -> [(Coord, Char)]
parseLine idx line = zip (getCoordsFromLine idx line) line

initEngine :: [(Coord, Char)] -> EngineSchematic
initEngine = foldr (\pair -> insert (fst pair) (snd pair)) empty

parseFile :: [String] -> EngineSchematic
parseFile file = initEngine (concat (zipWith parseLine (indexesFromZero file) file))
---

-- Helpers
indexes :: Int -> [a] -> [Int]
indexes _ [] = []
indexes n (_ : xs) = n : indexes (n + 1) xs

indexesFromZero :: [a] -> [Int]
indexesFromZero = indexes 0
---

executeDay3Part1 :: IO ()
executeDay3Part1 = do
  contents <- readFile "inputFiles/Day3.txt"
  let file = lines contents
  let eng = parseFile file
  print (sumPartNumbers eng)

executeDay3Part2 :: IO ()
executeDay3Part2 = do
  contents <- readFile "inputFiles/Day3.txt"
  let file = lines contents
  let engine = parseFile file
  print (executePart2 engine)
