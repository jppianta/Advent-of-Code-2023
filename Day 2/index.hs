data Color = Red | Green | Blue deriving (Show)

type Cube = (Int, Color)

-- Group of cubes for part two, representing (Red, Green Blue)
type GroupOfCubes = (Int, Int, Int)

-- A set of cubes is a tuple of the number of red, green and blue cubes
type SetOfCubes = [Cube]

-- A game is a tuple of the id of the game a a list of set of cubes
type Game = (Int, [SetOfCubes])

-- Part 1
isValidGame :: [SetOfCubes] -> Bool
isValidGame = all isValidSetOfCubes

isValidSetOfCubes :: SetOfCubes -> Bool
isValidSetOfCubes = all isValidCube

isValidCube :: Cube -> Bool
isValidCube (n, Red) = n <= 12
isValidCube (n, Green) = n <= 13
isValidCube (n, Blue) = n <= 14

extractValidGameId :: Game -> Int
extractValidGameId (id, game)
  | isValidGame game = id
  | otherwise = 0

sumValidGames :: [Game] -> Int
sumValidGames games = sum (map extractValidGameId games)
--

-- Part 2
powerOfGame :: Game -> Int
powerOfGame (_, game) = multiplyGroupOfCubes (minNumberOfCubesOnGame game)

multiplyGroupOfCubes :: GroupOfCubes -> Int
multiplyGroupOfCubes (r, g, b) = r * g * b

minNumberOfCubesOnGame :: [SetOfCubes] -> GroupOfCubes
minNumberOfCubesOnGame = foldr (\c acc -> minGroupOfCubes acc (minNumberOfSetOfCubes c)) (0, 0, 0)

minGroupOfCubes :: GroupOfCubes -> GroupOfCubes -> GroupOfCubes
minGroupOfCubes (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

minNumberOfSetOfCubes :: SetOfCubes -> GroupOfCubes
minNumberOfSetOfCubes = foldr minNumberOfCube (0, 0, 0)

minNumberOfCube :: Cube -> GroupOfCubes -> GroupOfCubes
minNumberOfCube (n, Red) (r, g, b) = (max n r, g, b)
minNumberOfCube (n, Green) (r, g, b) = (r, max n g, b)
minNumberOfCube (n, Blue) (r, g, b) = (r, g, max n b)

sumPowerOfGames :: [Game] -> Int
sumPowerOfGames games = sum (map powerOfGame games)

-- Parsers
parseGame :: String -> Game
parseGame s = (parseGameId s, parseGameInfo s)

parseGameId :: String -> Int
parseGameId s = extractGameId (head (splitOn ':' s))

-- Since the input here will be of type "Game n", we drop "Game " and parse the n
extractGameId :: String -> Int
extractGameId s = read (drop 5 s)

parseGameInfo :: String -> [SetOfCubes]
parseGameInfo s = parseGameSets (splitOn ':' s !! 1)

parseGameSets :: String -> [SetOfCubes]
parseGameSets s = map parseSetOfCubes (splitOn ';' s)

parseSetOfCubes :: String -> SetOfCubes
parseSetOfCubes s = map extractNumberOfCubes (splitOn ',' s)

-- The input will be of type "n Color", so we split on ' ', parse the n and parse the cube color with the Color
extractNumberOfCubes :: String -> Cube
extractNumberOfCubes s = (read (head x), parseCubeColor (x !! 1)) where
                            x = words (trimStart s)

parseCubeColor :: String -> Color
parseCubeColor s
  | s == "red" = Red
  | s == "blue" = Blue
  | s == "green" = Green
--

-- Helpers
splitOn :: Char -> String -> [String]
splitOn c s =  case dropWhile (==c) s of
                    "" -> []
                    s' -> w : splitOn c s''
                          where (w, s'') = break (==c) s'

trimStart :: String -> String
trimStart (x:xs)
  | x == ' ' = trimStart xs
  | otherwise = x:xs
--

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let singleLines = lines contents
  let games = map parseGame singleLines
  print (sumPowerOfGames games)
