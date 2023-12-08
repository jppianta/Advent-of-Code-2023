module Day8 where

import Data.HashMap hiding (filter, map, null)
import Prelude hiding (lookup)

type Direction = Char

type Directions = [Direction]

type Node = String

type NodePair = (Node, Node)

type Instructions = Map Node NodePair

type Document = (Directions, Instructions)

-- Part 1
stepsToZZZ :: Directions -> Instructions -> Int
stepsToZZZ dir inst = expandedStepsToZZZ dir inst "AAA" 0

expandedStepsToZZZ :: Directions -> Instructions -> Node -> Int -> Int
expandedStepsToZZZ _ _ "ZZZ" c = c
expandedStepsToZZZ [] _ _ c = c
expandedStepsToZZZ (x : xs) i n c = expandedStepsToZZZ xs i (getDirection x i n) (c + 1)

getDirection :: Direction -> Instructions -> Node -> Node
getDirection dir inst curr
  | dir == 'R' = snd (nodeOrAAA (lookup curr inst))
  | otherwise = fst (nodeOrAAA (lookup curr inst))
  where
    nodeOrAAA Nothing = ("AAA", "AAA")
    nodeOrAAA (Just n) = n

---

-- Par 2
stepsToZ :: Directions -> Instructions -> [Int]
stepsToZ dir inst = map (expandedStepsToZ dir inst 0) (nodesEndWithA inst)

expandedStepsToZ :: Directions -> Instructions -> Int -> Node -> Int
expandedStepsToZ [] _ c _ = c
expandedStepsToZ (x : xs) i c node
  | endWithZ node = c
  | otherwise = expandedStepsToZ xs i (c + 1) (getDirection x i node)

allEndWithZ :: [Node] -> Bool
allEndWithZ = all endWithZ

nodesEndWithA :: Instructions -> [Node]
nodesEndWithA = filter endWithA . keys

endWith :: Char -> Node -> Bool
endWith c = (== c) . last

endWithA :: Node -> Bool
endWithA = endWith 'A'

endWithZ :: Node -> Bool
endWithZ = endWith 'Z'

---

-- Parsers
parseDocument :: [String] -> Document
parseDocument [] = ([], empty)
parseDocument (x : xs) = (cycle x, parseInstructions (drop 1 xs))

parseInstructions :: [String] -> Instructions
parseInstructions = foldl (\inst s -> insert (take3 s) (take3 (drop 7 s), take3 (drop 12 s)) inst) empty
  where
    take3 = take 3

---

executeDay8Part1 :: IO ()
executeDay8Part1 = do
  contents <- readFile "inputFiles/Day8.txt"
  let doc = lines contents
  print (uncurry stepsToZZZ (parseDocument doc))

executeDay8Part2 :: IO ()
executeDay8Part2 = do
  contents <- readFile "inputFiles/Day8.txt"
  let file = lines contents
  let doc = parseDocument file
  print (foldr lcm 1 (uncurry stepsToZ doc))
