-- import Data.HashMap (HashSet, empty, insert, member, unions)

import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict (HashMap, empty, insert, member, (!))
import Data.List (elemIndex)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (trace)

data Tile = Ground | Vert | Horiz | NE | NW | SW | SE | G | S deriving (Enum, Show, Eq, Bounded)

data Direction = Up | Down | Lft | Rght deriving (Enum, Show, Eq, Bounded)

parse :: String -> ([[Tile]], (Int, Int))
parse str = (mp, findIndex mp)
  where
    mp = map (map findType) (lines str)

findIndex :: [[Tile]] -> (Int, Int)
findIndex mp
  | S `elem` head mp = (fromJust (S `elemIndex` head mp), 0)
  | otherwise = (fst (findIndex (tail mp)), snd (findIndex (tail mp)) + 1)

findType :: Char -> Tile
findType ch
  | ch == '.' = Ground
  | ch == '|' = Vert
  | ch == '-' = Horiz
  | ch == 'L' = NE
  | ch == 'J' = NW
  | ch == '7' = SW
  | ch == 'F' = SE
  | ch == 'S' = S
  | otherwise = S

printType :: Tile -> Char
printType ch
  | ch == Ground = '.'
  | ch == Vert = '|'
  | ch == Horiz = '-'
  | ch == NE = 'L'
  | ch == NW = 'J'
  | ch == SW = '7'
  | ch == SE = 'F'
  | ch == S = 'S'
  | otherwise = '.'

-- stuf :: (Array (Int, Int) Tile) -> Bool
-- stuf arr =

canTravel :: Direction -> Tile -> Tile -> Bool
canTravel dir tilePrev tileTravel = case dir of
  Up -> (tileTravel == Vert || tileTravel == NE || tileTravel == NW || tileTravel == S) && (tilePrev == Vert || tilePrev == SE || tilePrev == SW || tilePrev == S)
  Down -> (tileTravel == Vert || tileTravel == SE || tileTravel == SW || tileTravel == S) && (tilePrev == Vert || tilePrev == NE || tilePrev == NW || tilePrev == S)
  Lft -> (tileTravel == Horiz || tileTravel == SE || tileTravel == NE || tileTravel == S) && (tilePrev == Horiz || tilePrev == SW || tilePrev == NW || tilePrev == S)
  Rght -> (tileTravel == Horiz || tileTravel == SW || tileTravel == NW || tileTravel == S) && (tilePrev == Horiz || tilePrev == SE || tilePrev == NE || tilePrev == S)

travel :: (Int, Int) -> Direction -> (Int, Int)
travel (x, y) dir = case dir of
  Up -> (x, y + 1)
  Down -> (x, y - 1)
  Lft -> (x - 1, y)
  Rght -> (x + 1, y)

getTile :: [[Tile]] -> (Int, Int) -> Maybe Tile
getTile mp (x, y) =
  if x < 0 || y < 0 || x >= length (head mp) || y >= length mp
    then Nothing
    else Just ((mp !! y) !! x)

dfs :: [[Tile]] -> Bool -> (Int, Int) -> (Int, Int) -> Maybe Int
dfs mp firstRun pos prev_pos
  | currentTile == S && not firstRun = Just 0
  | null possibleTiles = Nothing `debug` (show possibleTiles ++ " " ++ show pos)
  | null newTiles = Nothing `debug` (show possibleTiles ++ " " ++ show pos)
  | otherwise = Just (head newTiles + 1) `debug` (show possibleTiles ++ " " ++ show pos)
  where
    currentTile = fromJust (getTile mp pos)
    possibleTiles = filter (\(d, newPos, tile) -> canTravel d currentTile tile && newPos /= prev_pos) (mapMaybe (mapTravel mp pos) [minBound .. maxBound])
    newTiles = mapMaybe (\(_, newPos, _) -> dfs mp False newPos pos) possibleTiles

dfsP2 :: [[Tile]] -> HashMap (Int, Int) Tile -> Bool -> (Int, Int) -> (Int, Int) -> HashMap (Int, Int) Tile
dfsP2 mp tiles firstRun pos prev_pos
  | currentTile == S && not firstRun = insert pos currentTile tiles
  | null possibleTiles = tiles `debug` (show possibleTiles ++ " " ++ show pos)
  | null newTiles = tiles `debug` (show possibleTiles ++ " " ++ show pos)
  | otherwise = insert pos currentTile (head newTiles) `debug` (show possibleTiles ++ " " ++ show pos)
  where
    currentTile = fromJust (getTile mp pos)
    possibleTiles = filter (\(d, newPos, tile) -> canTravel d currentTile tile && newPos /= prev_pos) (mapMaybe (mapTravel mp pos) [minBound .. maxBound])
    newTiles = map (\(_, newPos, _) -> dfsP2 mp tiles False newPos pos) possibleTiles

debug = flip trace

mapTravel :: [[Tile]] -> (Int, Int) -> Direction -> Maybe (Direction, (Int, Int), Tile)
mapTravel mp pos dir = case getTile mp newPos of
  Just a -> Just (dir, newPos, a)
  Nothing -> Nothing
  where
    newPos = travel pos dir

-- floodfill :: HashSet (Int, Int) -> HashSet (Int, Int) -> (Int, Int) -> (Int, Int) -> HashSet (Int, Int)
-- floodfill tiles found (width, height) (x, y)
--   | x < -1 || x > width = found
--   | y < -1 || y > height = found
--   | (x, y) `member` tiles = found
--   | (x, y) `member` found = found
--   | otherwise = unions (map (floodfill tiles ((x, y) `insert` found) (width, height)) (filter (`member` found) (map (travel (x, y)) [minBound .. maxBound])) `debug` show (x, y))
-- findType ch
--   | ch == '.' = Ground
--   | ch == '|' = Vert
--   | ch == '-' = Horiz
--   | ch == 'L' = NE
--   | ch == 'J' = NW
--   | ch == '7' = SW
--   | ch == 'F' = SE
--   | ch == 'S' = S
--   | otherwise = S

raycast :: HashMap (Int, Int) Tile -> Int -> Int -> Int -> Bool -> Tile -> Int
raycast tiles y width x countInside lastNonHorizTile
  | x >= width = 0
  | (x, y) `member` tiles && tiles ! (x, y) == Vert = raycast tiles y width (x + 1) (not countInside) Vert
  | (x, y) `member` tiles && tiles ! (x, y) == S = raycast tiles y width (x + 1) (not countInside) Vert -- This is specific to my input, the S acts like a vertical line.
  | (x, y) `member` tiles && tiles ! (x, y) == Horiz = raycast tiles y width (x + 1) countInside lastNonHorizTile
  -- Always east to west
  | (x, y) `member` tiles && lastNonHorizTile == SE && tiles ! (x, y) == NW = raycast tiles y width (x + 1) (not countInside) (tiles ! (x, y)) -- F J
  | (x, y) `member` tiles && lastNonHorizTile == NE && tiles ! (x, y) == SW = raycast tiles y width (x + 1) (not countInside) (tiles ! (x, y)) -- L 7

  | (x, y) `member` tiles && lastNonHorizTile == SE && tiles ! (x, y) == SW = raycast tiles y width (x + 1) countInside Vert -- F J
  | (x, y) `member` tiles && lastNonHorizTile == NE && tiles ! (x, y) == NW = raycast tiles y width (x + 1) countInside Vert -- 
  | (x, y) `member` tiles = raycast tiles y width (x + 1) countInside (tiles ! (x, y))

  | countInside = raycast tiles y width (x + 1) countInside lastNonHorizTile + 1
  | otherwise = raycast tiles y width (x + 1) countInside lastNonHorizTile

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let (mp, start) = parse contents
  let tiles = dfsP2 mp empty True start start
  print (length tiles `div` 2)
  putStrLn (unlines [[if (x, y) `member` tiles then printType (tiles ! (x, y)) else '.' | x <- [0 .. length (head mp) - 1]] | y <- [0..length mp]])
  print (sum [raycast tiles y (length (head mp)) 0 False Vert | y <- [0 .. (length mp - 1)]])

-- print (floodfill tiles Data.HashSet.empty (length (head mp), length mp) (-1, -1))
