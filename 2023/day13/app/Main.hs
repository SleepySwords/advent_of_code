import Data.List.Split (splitOn)
import Debug.Trace (trace)

data Tile = Ash | Ground deriving (Show, Enum, Eq)

parse :: String -> [[Tile]]
parse str = map (map getTile) $ lines str

getTile :: Char -> Tile
getTile ch = if ch == '#' then Ash else Ground

hasReflectionVertical :: [[Tile]] -> Int -> Bool
hasReflectionVertical tiles index = all (\a -> rightMax a == leftMax a) tiles
  where
    left = take index
    right = drop index
    maxLength tile = min (length (right tile)) (length (left tile))
    rightMax tile = reverse (take (maxLength tile) (right tile))
    leftMax tile = drop (length (left tile) - maxLength tile) (left tile)

smudges :: [[Tile]] -> Int -> Int
smudges tiles index = sum (map (\a -> numberNotEquals (rightMax a) (leftMax a)) tiles)
  where
    left = take index
    right = drop index
    maxLength tile = min (length (right tile)) (length (left tile))
    rightMax tile = reverse (take (maxLength tile) (right tile))
    leftMax tile = drop (length (left tile) - maxLength tile) (left tile)

-- Assumed the same size
numberNotEquals :: [Tile] -> [Tile] -> Int
numberNotEquals [] [] = 0
numberNotEquals [] _ = 0 -- Theses should not exist but linter complains
numberNotEquals _ [] = 0 -- Theses should not exist but linter complains
numberNotEquals (hl : left) (hr : right) = if hl == hr then numberNotEquals left right else numberNotEquals left right + 1

debug = flip trace

rotate :: [[Tile]] -> [[Tile]]
rotate x
  | all null x = []
rotate x = map head x : rotate (map tail x)

getTiles :: [[Tile]] -> Int
getTiles mp = if not (null vert) then head vert else head horiz * 100
  where
    vert = filter (hasReflectionVertical mp) [1 .. length (head mp) - 1]
    horiz = filter (hasReflectionVertical (rotate mp)) [1 .. length mp - 1]

findSmudge :: [[Tile]] -> Int
findSmudge mp = if not (null vert) then head vert else head horiz * 100
  where
    vert = filter (\a -> smudges mp a == 1) [1 .. length (head mp) - 1]
    horiz = filter (\a -> smudges (rotate mp) a == 1) [1 .. length mp - 1]

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let maps = map parse $ splitOn "\n\n" contents
  putStrLn $ "Part 1: " ++ show (sum $ map getTiles maps)
  putStrLn $ "Part 2: " ++ show (sum $ map findSmudge maps)
