module Main where

import Data.Char (isSpace)
import Data.List.Split (splitOn)

parseCard :: String -> (Int, ([Int], [Int]))
parseCard a = (read (dropWhile isSpace (drop 4 (head splits))), parseWinnning (last splits))
  where
    splits = splitOn ":" a

parseWinnning :: String -> ([Int], [Int])
parseWinnning a = (splitCards (head splits), splitCards (last splits))
  where
    splits = splitOn " |" a

splitCards :: String -> [Int]
splitCards [] = []
splitCards st = fst splits : splitCards (snd splits)
  where
    splits = parseFraction (splitAt 3 st)

parseFraction :: (String, String) -> (Int, String)
parseFraction (a, b) = (read (dropWhile isSpace a), b)

findNumberWinningCards :: ([Int], [Int]) -> Int
findNumberWinningCards (winning, has) = length (filter (`elem` winning) has)

convertPoints :: Int -> Int
convertPoints 0 = 0
convertPoints a = 2 ^ (a - 1)

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  print ("Part 1: " ++ show (sum (map (convertPoints . (findNumberWinningCards . snd) . parseCard) (lines contents))))

calculateCopiesList :: [Int] -> [Int] -> Int -> [Int]
calculateCopiesList cardsWonList numberOfCardsList index
  | index == length numberOfCardsList = numberOfCardsList
  | otherwise = calculateCopiesList cardsWonList c (index + 1)
  where c = [addOffset cardsWonList numberOfCardsList index x | x <- [0..length numberOfCardsList - 1]]

addOffset :: [Int] -> [Int] -> Int -> Int -> Int
addOffset cardsWonList numberOfCardsList index findIndex = if index < findIndex && (index + cardsWonList!!index) >= findIndex
    then numberOfCardsList!!findIndex + numberOfCardsList!!index
    else numberOfCardsList!!findIndex

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let cardsWonList = map ((findNumberWinningCards . snd) . parseCard) (lines contents)
  print ("Part 2: " ++ show(sum (calculateCopiesList cardsWonList [1 | _ <- [0 .. length cardsWonList - 1]] 0)))

main :: IO ()
main = do
  part1
  part2
