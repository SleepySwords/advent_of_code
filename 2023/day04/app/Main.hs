module Main where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

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

matches :: ([Int], [Int]) -> Int
matches (winning, has) = length (filter (fil winning) has)

fil :: [Int] -> Int -> Bool
fil arr el = el `elem` arr

findPoints :: Int -> Int
findPoints 0 = 0
findPoints a = 2 ^ (a - 1)

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  print (sum (map (findPoints . (matches . snd) . parseCard) (lines contents)))

calculatePart2 :: [Int] -> [Int] -> Int -> [Int]
calculatePart2 toAdd curr index
  | index == length curr = curr
  | otherwise = calculatePart2 toAdd c (index + 1)
  where c = [addOffset toAdd curr index x | x <- [0..length curr - 1]]

addOffset :: [Int] -> [Int] -> Int -> Int -> Int
addOffset toAdd curr index findIndex = if index < findIndex && (index + toAdd!!index) >= findIndex
    then curr!!findIndex + curr!!index
    else curr!!findIndex

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let toAdd = map ((matches . snd) . parseCard) (lines contents)
  print (sum (calculatePart2 toAdd [1 | _ <- [0 .. length toAdd - 1]] 0))

main :: IO ()
main = do
  part1
  part2
