module Main where

import Data.Function (on)
import Data.List (group, maximumBy, sort, sortBy)

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let bets = map words (lines contents)
  part1 bets
  part2 bets

part1 :: [[String]] -> IO ()
part1 bets = do
  let sorted = sortBy (compareBidsPart1 `on` head) bets
  putStrLn $ "Part 1: " ++ show (addValues sorted)

part2 :: [[String]] -> IO ()
part2 bets = do
  let sorted = sortBy (compareBidsPart2 `on` head) bets
  putStrLn $ "Part 2: " ++ show (addValues sorted)

addValues :: [[String]] -> Int
addValues sorted = sum [read (last v) * (i + 1) | (i, v) <- zip [0..] sorted]

compareBidsPart1 :: String -> String -> Ordering
compareBidsPart1 fstC sndC =
  if t == EQ
    then map values fstC `compare` map values sndC
    else t
  where
    t = findType fstC `compare` findType sndC

compareBidsPart2 :: String -> String -> Ordering
compareBidsPart2 fstC sndC =
  if t == EQ
    then map valuesPart2 fstC `compare` map valuesPart2 sndC
    else t
  where
    t = findTypePart2 fstC `compare` findTypePart2 sndC

data House = High | One | Two | Three | FullHouse | Four | Five deriving (Enum, Eq, Show, Ord)

findType :: String -> House
findType card
  | 5 `elem` list = Five
  | 4 `elem` list = Four
  | 3 `elem` list && 2 `elem` list = FullHouse
  | 3 `elem` list = Three
  | length (filter (== 2) list) == 2 = Two
  | 2 `elem` list = One
  | otherwise = High
  where
    list = map length (group (sort card))

findTypePart2 :: String -> House
findTypePart2 card
  | null list = Five
  | otherwise = findType (filter (/= 'J') card ++ [fst (maximumBy (compare `on` snd) list) | _ <- filter (== 'J') card])
  where
    list = map (\l -> (head l, length l)) $ filter ((/= 'J') . head) (group (sort card))

values :: Char -> Int
values c
  | c == 'A' = 12
  | c == 'K' = 11
  | c == 'Q' = 10
  | c == 'J' = 9
  | c == 'T' = 8
  | c == '9' = 7
  | c == '8' = 6
  | c == '7' = 5
  | c == '6' = 4
  | c == '5' = 3
  | c == '4' = 2
  | c == '3' = 1
  | c == '2' = 0
  | otherwise = -1

valuesPart2 :: Char -> Int
valuesPart2 c
  | c == 'A' = 12
  | c == 'K' = 11
  | c == 'Q' = 10
  | c == 'T' = 9
  | c == '9' = 8
  | c == '8' = 7
  | c == '7' = 6
  | c == '6' = 5
  | c == '5' = 4
  | c == '4' = 3
  | c == '3' = 2
  | c == '2' = 1
  | c == 'J' = 0
  | otherwise = -1
