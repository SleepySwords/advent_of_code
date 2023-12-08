import Data.HashSet (HashSet, empty, insert, member)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList)
import Data.Map.Strict (lookup)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Prelude hiding (lookup)

parseBracket :: String -> (String, String)
parseBracket str = (take 3 (head w), take 3 (last w))
  where
    w = words (drop 1 str)

parse :: String -> [(String, (String, String))]
parse cont = map (\a -> let spt = Data.List.Split.splitOn " = " a in (head spt, parseBracket (last spt))) $ lines cont

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let spt = Data.List.Split.splitOn "\n\n" contents
  let network = fromList (parse (last spt))
  putStrLn ("Part 1: " ++ show (traverseThing network "AAA" (cycle (head spt))))
  let arr = filter (isSuffixOf "A") (map fst (parse (last spt)))
  -- putStrLn ("Part 2: " ++ show (lcmm (map (fst . cycleLength network (cycle (head spt)) []) arr)))

  -- This assumes the input does not repeat, it also assumes the length from the first element to Z is the same for every subsequent loop.
  putStrLn ("Part 2: " ++ show (lcmm (map (cycleLengthOpt network (cycle (zip (head spt) [0 ..])) Data.HashSet.empty) arr)))

lcmm :: [Int] -> Int
lcmm = foldr lcm 1

traverseThing :: Map String (String, String) -> String -> [Char] -> Int
traverseThing _ "ZZZ" _ = 0
traverseThing network curr directions = traverseThing network (traverseNet network (head directions) curr) (tail directions) + 1

traverseThingP2 :: Map String (String, String) -> [String] -> [Char] -> Int
traverseThingP2 network currents directions
  | all (isSuffixOf "Z") currents = 0
  | otherwise = traverseThingP2 network (map (traverseNet network (head directions)) currents) (tail directions) + 1

debug = flip trace

traverseNet :: Map String (String, String) -> Char -> String -> String
traverseNet network dir curr =
  if dir == 'L'
    then fst $ fromJust (lookup curr network)
    else snd $ fromJust (lookup curr network)

cycleLength :: Map String (String, String) -> [Char] -> [(String, [Char])] -> String -> (Int, Int)
cycleLength network dir arr curr
  | "Z" `isSuffixOf` curr = (0, snd c + 1) `debug` "Tst"
  | (curr, dir) `elem` arr = (0, 0) `debug` "True"
  | otherwise = (fst c + 1, snd c + 1)
  where
    next = traverseNet network (head dir) curr
    c = cycleLength network (tail dir) ((curr, dir) : arr) next -- `debug` show next

cycleLengthOpt :: Map String (String, String) -> [(Char, Int)] -> HashSet (String, Int) -> String -> Int
cycleLengthOpt network dir arr curr
  | member (curr, snd (head dir)) arr = 0
  | "Z" `isSuffixOf` curr = 0
  | otherwise = c + 1
  where
    next = traverseNet network (fst $ head dir) curr
    c = cycleLengthOpt network (tail dir) (insert (curr, snd (head dir)) arr) next -- `debug` show next
