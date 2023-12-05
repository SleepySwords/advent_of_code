module Main where

import Data.List.Split (splitOn)
import Data.Range (inRanges, (+=*))

main :: IO ()
main = do
  part1
  part2

parseSeed :: String -> [Int]
parseSeed seeds = map read (splitOn " " (drop 7 seeds))

parseSeedPairs :: String -> [(Int, Int)]
parseSeedPairs seeds = [(splits !! x, splits !! (x + 1)) | x <- [0 .. (length splits - 1)], even x]
  where
    splits = map read (splitOn " " (drop 7 seeds))

parseMap :: String -> (Int, Int, Int)
parseMap maps = (head splits, splits !! 1, splits !! 2)
  where
    splits = map read (splitOn " " maps)

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let splits = splitOn "\n\n" contents
  let seeds = parseSeed $ head splits
  let maps = map (map parseMap . tail . lines) $ tail splits

  putStrLn $ "Part 1: " ++ show (minimum (map (applyMaps maps) seeds))

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let splits = splitOn "\n\n" contents
  let seeds = parseSeedPairs $ head splits
  let maps = map (map parseMap . tail . lines) $ tail splits

  let m = reverse $ map (map reverseTransformation) maps

  let ranges = concat [[seed Data.Range.+=* (seed + offset)] | (seed, offset) <- seeds]

  let value = head (filter (inRanges ranges) (map (applyMaps m) [0 ..]))
  putStrLn $ "Part 2: " ++ show (applyMaps maps value)

applyMaps :: [[(Int, Int, Int)]] -> Int -> Int
applyMaps [] value = value
applyMaps arr value = applyMaps (tail arr) $ transform (head arr) value

transform :: [(Int, Int, Int)] -> Int -> Int
transform tranformations value = applyTransformation value $ filter (isInLength value) tranformations

isInLength :: Int -> (Int, Int, Int) -> Bool
isInLength value (_, source, len) = value >= source && value < source + len

applyTransformation :: Int -> [(Int, Int, Int)] -> Int
applyTransformation value [] = value
applyTransformation value arr = applyTransformation' value $ head arr

applyTransformation' :: Int -> (Int, Int, Int) -> Int
applyTransformation' value (dest, source, _) = dest + (value - source)

reverseTransformation :: (Int, Int, Int) -> (Int, Int, Int)
reverseTransformation (dest, source, len) = (source, dest, len)
