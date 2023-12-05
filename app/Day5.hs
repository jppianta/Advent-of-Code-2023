module Day5 where

import Data.List (sortOn, foldl')
import Utils (splitOn)

type DestinationRange = Int
type SourceRange = Int
type RangeLenght = Int
type Seed = Int
type SeedPair = (Seed, Int)

type RangeGroup = (DestinationRange, SourceRange, RangeLenght)

type Map = [RangeGroup]
type Seeds = [Seed]
type SeedPairs = [SeedPair]
type Almanac = (Seeds, [Map])
type PairAlmanac = (SeedPairs, [Map])

-- Part 1
lowestLocation :: Almanac -> Int
lowestLocation = smallestInt . getLocations

getLocations :: Almanac -> [Int]
getLocations (seeds, maps) = map (`getLocation` maps) seeds

getLocation :: Seed -> [Map] -> Int
getLocation seed maps = foldl correspondingValue (correspondingValue seed (head maps)) (tail maps)

correspondingValue :: Int -> Map -> Int
correspondingValue n [] = n
correspondingValue n ((dest, source, len):xs)
  | n >= source && n < source + len = dest + (n - source)
  | otherwise = correspondingValue n xs
---

-- Part 2
lowestFromPairs :: PairAlmanac -> Int
lowestFromPairs = smallestInt . (map fst) . correspondingPairsFromAlmanac

correspondingPairsFromAlmanac :: PairAlmanac -> SeedPairs
correspondingPairsFromAlmanac (seedPairs, maps) = foldl getCorrespondingPairs [head seedPairs] maps

getCorrespondingPairs :: SeedPairs -> Map -> SeedPairs
getCorrespondingPairs pairs maps = concat (map (`correspondingPairs` maps) pairs)

correspondingPairs :: SeedPair -> Map -> SeedPairs
correspondingPairs n [] = [n]
correspondingPairs (seed, slen) ((dest, source, len):xs)
  | seed >= source && seed < source + len && seed + slen >= source + len = (nextStart, lenDiff) : correspondingPairs (seed + lenDiff, slen - lenDiff) xs
  | seed >= source && seed < source + len = [(nextStart, slen)]
  | otherwise = correspondingPairs (seed, slen) xs
  where nextStart = dest + (seed - source)
        lenDiff = (source + len) - seed
---

-- Parsers
parseFile :: [[String]] -> Almanac
parseFile [] = ([], [])
parseFile (x:xs) = (parseSeeds (concat x), parseMaps xs)

parseFilePart2 :: [[String]] -> PairAlmanac
parseFilePart2 [] = ([], [])
parseFilePart2 (x:xs) = (parseSeedPairs (concat x), parseMaps xs)

parseSeeds :: String -> Seeds
parseSeeds s = map read (words (drop 7 s))

parseSeedPairs :: String -> [SeedPair]
parseSeedPairs = pairUp . parseSeeds

parseMaps :: [[String]] -> [Map]
parseMaps = map parseMap

parseMap :: [String] -> Map
parseMap = parseRanges . drop 1

parseRanges :: [String] -> Map
parseRanges = (sortOn sourceRange) . (map parseRange)

sourceRange :: RangeGroup -> SourceRange
sourceRange (_, s, _) = s

parseRange :: String -> RangeGroup
parseRange s = (read x, read y, read z)
  where
    (x:y:z:_) = words s
---

-- Utils
smallestInt :: [Int] -> Int
smallestInt = foldl' min maxBound

pairUp :: [a] -> [(a, a)]
pairUp (x:y:xs) = (x, y) : pairUp xs
pairUp [] = []
pairUp (x:_) = [(x, x)]
---

executeDay5Part1 :: IO ()
executeDay5Part1 = do
  contents <- readFile "inputFiles/Day5.txt"
  let file = lines contents
  let groups = splitOn "" file
  let almanac = parseFile groups
  print (lowestLocation almanac)

executeDay5Part2 :: IO ()
executeDay5Part2 = do
  contents <- readFile "inputFiles/Day5.txt"
  let file = lines contents
  let groups = splitOn "" file
  let almanac = parseFilePart2 groups
  print (lowestFromPairs almanac)
