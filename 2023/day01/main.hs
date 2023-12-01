import Data.Char (isDigit)
import Data.Function (on)
import Data.List (findIndex, inits, isPrefixOf, isSuffixOf, maximumBy, minimumBy, tails)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust)

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let linesOfFile = lines contents
  let digits = map mapDigitToPart1 linesOfFile
  print (sum digits)

mapDigitToPart1 :: String -> Int
mapDigitToPart1 xs = read (head [[x] | x <- xs, isDigit x]) * 10 + read (last [[x] | x <- xs, isDigit x]) :: Int

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let linesOfFile = lines contents
  print (sum (map mapDigitToPart2 linesOfFile))

numbersList = [("one", 1), ("1", 1), ("two", 2), ("2", 2), ("three", 3), ("3", 3), ("four", 4), ("4", 4), ("five", 5), ("5", 5), ("six", 6), ("6", 6), ("seven", 7), ("7", 7), ("eight", 8), ("8", 8), ("nine", 9), ("9", 9)]

mapDigitToPart2 :: String -> Int
mapDigitToPart2 xs = snd (findFirstDigit xs) * 10 + snd (findLastDigit xs)

findFirstDigit :: String -> (Int, Int)
findFirstDigit str = minimumBy (comparing fst) [(fromJust (findSubstring ptrn str), value) | (ptrn, value) <- numbersList, isJust (findSubstring ptrn str)]

findSubstring :: (Eq a) => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str)

findLastDigit :: String -> (Int, Int)
findLastDigit str = minimumBy (comparing fst) [(fromJust (findRSubstring pat str), value) | (pat, value) <- numbersList, isJust (findRSubstring pat str)]

findRSubstring :: (Eq a) => [a] -> [a] -> Maybe Int
findRSubstring ptrn str = findIndex (isSuffixOf ptrn) (reverse (inits str))
