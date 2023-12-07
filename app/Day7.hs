{-# LANGUAGE InstanceSigs #-}

module Day7 where

import Data.Char (isDigit)
import Data.List (sort)

data CamalCard = Figure Char | Number Int deriving (Show)

camalCardFigureOrder :: [Char]
camalCardFigureOrder = ['A', 'K', 'Q', 'J', 'T']

instance Eq CamalCard where
  (Figure f1) == (Figure f2) = f1 == f2
  (Number n1) == (Number n2) = n1 == n2
  (Figure _) == (Number _) = False
  (Number _) == (Figure _) = False

instance Ord CamalCard where
  compare (Figure f1) (Figure f2) = order f1 f2 camalCardFigureOrder
  compare (Number n1) (Number n2) = compare n1 n2
  compare (Figure _) (Number _) = GT
  compare (Number _) (Figure _) = LT

data CamalCardWithJoker = F Char | N Int | Joker deriving (Show)

camalCardWithJokerFigureOrder :: [Char]
camalCardWithJokerFigureOrder = ['A', 'K', 'Q', 'T']

instance Eq CamalCardWithJoker where
  Joker == Joker = True
  Joker == _ = False
  _ == Joker = False
  (F f1) == (F f2) = f1 == f2
  (N n1) == (N n2) = n1 == n2
  (F _) == (N _) = False
  (N _) == (F _) = False

instance Ord CamalCardWithJoker where
  compare (F f1) (F f2) = order f1 f2 camalCardWithJokerFigureOrder
  compare (N n1) (N n2) = compare n1 n2
  compare (F _) (N _) = GT
  compare (N _) (F _) = LT
  compare Joker Joker = EQ
  compare Joker _ = LT
  compare _ Joker = GT

data HandType = FiveOfKind | FourOfKind | FullHouse | ThreeOfKind | TwoPair | Pair | Single deriving (Show)

instance Eq HandType where
  FiveOfKind == FiveOfKind = True
  FourOfKind == FourOfKind = True
  FullHouse == FullHouse = True
  ThreeOfKind == ThreeOfKind = True
  TwoPair == TwoPair = True
  Pair == Pair = True
  Single == Single = True
  _ == _ = False

handOrder :: [HandType]
handOrder = [FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPair, Pair, Single]

instance Ord HandType where
  compare :: HandType -> HandType -> Ordering
  compare a b = order a b handOrder

compareSingleCards :: [CamalCard] -> [CamalCard] -> Ordering
compareSingleCards [] [] = EQ
compareSingleCards _ [] = EQ
compareSingleCards [] _ = EQ
compareSingleCards (x : xs) (y : ys)
  | x == y = compareSingleCards xs ys
  | otherwise = compare x y

compareSingleCardsWithJoker :: [CamalCardWithJoker] -> [CamalCardWithJoker] -> Ordering
compareSingleCardsWithJoker [] [] = EQ
compareSingleCardsWithJoker _ [] = EQ
compareSingleCardsWithJoker [] _ = EQ
compareSingleCardsWithJoker (x : xs) (y : ys)
  | x == y = compareSingleCardsWithJoker xs ys
  | otherwise = compare x y

data Hand = H HandType [CamalCard] deriving (Show)

instance Eq Hand where
  (H ht1 _) == (H ht2 _) = ht1 == ht2

instance Ord Hand where
  compare (H ht1 c1) (H ht2 c2)
    | ht1 == ht2 = compareSingleCards c1 c2
    | otherwise = compare ht1 ht2

data HandWithJoker = HJ HandType [CamalCardWithJoker] deriving (Show)

instance Eq HandWithJoker where
  (HJ ht1 _) == (HJ ht2 _) = ht1 == ht2

instance Ord HandWithJoker where
  compare (HJ ht1 c1) (HJ ht2 c2)
    | ht1 == ht2 = compareSingleCardsWithJoker c1 c2
    | otherwise = compare ht1 ht2

type Bid = Int

type Game = (Hand, Bid)

type GameWithJoker = (HandWithJoker, Bid)

-- Part 1
sumOfBids :: [Game] -> Int
sumOfBids = (`sumOfBidsWithCounter` 1)

sumOfBidsWithCounter :: [Game] -> Int -> Int
sumOfBidsWithCounter [] _ = 0
sumOfBidsWithCounter ((_, bid) : xs) c = bid * c + sumOfBidsWithCounter xs (c + 1)

getHand :: [CamalCard] -> Hand
getHand cards = H (getHandType (countCards cards)) cards

getHandType :: [(CamalCard, Int)] -> HandType
getHandType cards
  | hasFiveOfKind cards = FiveOfKind
  | hasFourOfKind cards = FourOfKind
  | hasFullHouse cards = FullHouse
  | hasThreeOfKind cards = ThreeOfKind
  | hasTwoPair cards = TwoPair
  | hasPair cards = Pair
  | otherwise = Single

hasFiveOfKind :: [(CamalCard, Int)] -> Bool
hasFiveOfKind = any ((== 5) . snd)

hasFourOfKind :: [(CamalCard, Int)] -> Bool
hasFourOfKind = any ((== 4) . snd)

hasFullHouse :: [(CamalCard, Int)] -> Bool
hasFullHouse cards = hasThreeOfKind cards && hasPair cards

hasThreeOfKind :: [(CamalCard, Int)] -> Bool
hasThreeOfKind = any ((== 3) . snd)

hasTwoPair :: [(CamalCard, Int)] -> Bool
hasTwoPair = (== 2) . count ((== 2) . snd)

hasPair :: [(CamalCard, Int)] -> Bool
hasPair = any ((== 2) . snd)

countCards :: [CamalCard] -> [(CamalCard, Int)]
countCards = foldl addCardToCount []

addCardToCount :: [(CamalCard, Int)] -> CamalCard -> [(CamalCard, Int)]
addCardToCount [] card = [(card, 1)]
addCardToCount ((c, n) : xs) card
  | c == card = (c, n + 1) : xs
  | otherwise = (c, n) : addCardToCount xs card

---

-- Part 2
sumOfBidsWithJoker :: [GameWithJoker] -> Int
sumOfBidsWithJoker = (`sumOfBidsWithCounterJ` 1)

sumOfBidsWithCounterJ :: [GameWithJoker] -> Int -> Int
sumOfBidsWithCounterJ [] _ = 0
sumOfBidsWithCounterJ ((_, bid) : xs) c = bid * c + sumOfBidsWithCounterJ xs (c + 1)

getHandWithJoker :: [CamalCardWithJoker] -> HandWithJoker
getHandWithJoker cards = HJ (getHandTypeWithJoker (countCardsWithJoker cards)) cards

getHandTypeWithJoker :: ([(CamalCardWithJoker, Int)], (CamalCardWithJoker, Int)) -> HandType
getHandTypeWithJoker (cards, j)
  | hasFiveOfKindJ cards = upgradeWithJokers FiveOfKind j
  | hasFourOfKindJ cards = upgradeWithJokers FourOfKind j
  | hasFullHouseJ cards = upgradeWithJokers FullHouse j
  | hasThreeOfKindJ cards = upgradeWithJokers ThreeOfKind j
  | hasTwoPairJ cards = upgradeWithJokers TwoPair j
  | hasPairJ cards = upgradeWithJokers Pair j
  | otherwise = upgradeWithJokers Single j

hasFiveOfKindJ :: [(CamalCardWithJoker, Int)] -> Bool
hasFiveOfKindJ = any ((== 5) . snd)

hasFourOfKindJ :: [(CamalCardWithJoker, Int)] -> Bool
hasFourOfKindJ = any ((== 4) . snd)

hasFullHouseJ :: [(CamalCardWithJoker, Int)] -> Bool
hasFullHouseJ cards = hasThreeOfKindJ cards && hasPairJ cards

hasThreeOfKindJ :: [(CamalCardWithJoker, Int)] -> Bool
hasThreeOfKindJ = any ((== 3) . snd)

hasTwoPairJ :: [(CamalCardWithJoker, Int)] -> Bool
hasTwoPairJ = (== 2) . count ((== 2) . snd)

hasPairJ :: [(CamalCardWithJoker, Int)] -> Bool
hasPairJ = any ((== 2) . snd)

upgradeWithJokers :: HandType -> (CamalCardWithJoker, Int) -> HandType
upgradeWithJokers FiveOfKind _ = FiveOfKind
upgradeWithJokers FullHouse _ = FullHouse
upgradeWithJokers FourOfKind (Joker, 1) = FiveOfKind
upgradeWithJokers FourOfKind _ = FourOfKind
upgradeWithJokers ThreeOfKind (Joker, 1) = FourOfKind
upgradeWithJokers ThreeOfKind (Joker, 2) = FiveOfKind
upgradeWithJokers ThreeOfKind _ = ThreeOfKind
upgradeWithJokers TwoPair (Joker, 1) = FullHouse
upgradeWithJokers TwoPair _ = TwoPair
upgradeWithJokers Pair (Joker, 1) = ThreeOfKind
upgradeWithJokers Pair (Joker, 2) = FourOfKind
upgradeWithJokers Pair (Joker, 3) = FiveOfKind
upgradeWithJokers Pair _ = Pair
upgradeWithJokers Single (Joker, 1) = Pair
upgradeWithJokers Single (Joker, 2) = ThreeOfKind
upgradeWithJokers Single (Joker, 3) = FourOfKind
upgradeWithJokers Single (Joker, 4) = FiveOfKind
upgradeWithJokers Single (Joker, 5) = FiveOfKind
upgradeWithJokers Single _ = Single

countCardsWithJoker :: [CamalCardWithJoker] -> ([(CamalCardWithJoker, Int)], (CamalCardWithJoker, Int))
countCardsWithJoker = foldl addCardToCountWithJoker ([], (Joker, 0))

addCardToCountWithJoker :: ([(CamalCardWithJoker, Int)], (CamalCardWithJoker, Int)) -> CamalCardWithJoker -> ([(CamalCardWithJoker, Int)], (CamalCardWithJoker, Int))
addCardToCountWithJoker (l, (jk, c)) Joker = (l, (jk, c + 1))
addCardToCountWithJoker ([], j) card = ([(card, 1)], j)
addCardToCountWithJoker ((c, n) : xs, j) card
  | c == card = ((c, n + 1) : xs, j)
  | otherwise = ((c, n) : fst r, snd r)
  where
    r = addCardToCountWithJoker (xs, j) card

---

-- Utils
order :: (Eq a) => a -> a -> [a] -> Ordering
order _ _ [] = EQ
order a b (x : xs)
  | a == b = EQ
  | a == x = GT
  | b == x = LT
  | otherwise = order a b xs

count :: (a -> Bool) -> [a] -> Int
count f a = length (filter f a)

---

-- Parsers
parseGame :: String -> Game
parseGame s = (getHand (map charToCamalCard h), read b) where (h : b : _) = words s

parseCamalCards :: String -> [CamalCard]
parseCamalCards = map charToCamalCard

charToCamalCard :: Char -> CamalCard
charToCamalCard c
  | isDigit c = Number (read [c])
  | otherwise = Figure c

parseGameWithJoker :: String -> GameWithJoker
parseGameWithJoker s = (getHandWithJoker (map charToCamalCardWithJoker h), read b) where (h : b : _) = words s

parseCamalCardsWithJoker :: String -> [CamalCardWithJoker]
parseCamalCardsWithJoker = map charToCamalCardWithJoker

charToCamalCardWithJoker :: Char -> CamalCardWithJoker
charToCamalCardWithJoker c
  | isDigit c = N (read [c])
  | c == 'J' = Joker
  | otherwise = F c

---

executeDay7Part1 :: IO ()
executeDay7Part1 = do
  contents <- readFile "inputFiles/Day7.txt"
  let singleLines = lines contents
  let games = map parseGame singleLines
  print (sumOfBids (sort games))

executeDay7Part2 :: IO ()
executeDay7Part2 = do
  contents <- readFile "inputFiles/Day7.txt"
  let singleLines = lines contents
  let games = map parseGameWithJoker singleLines
  print (sumOfBidsWithJoker (sort games))