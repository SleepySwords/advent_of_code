-- import Data.HashMap (HashSet, empty, insert, member, unions)

import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict (empty, insert, member, (!))
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust, isJust, mapMaybe)

data Tile = Ground | Vert | Horiz | NE | NW | SW | SE | G | S deriving (Enum, Show, Eq, Bounded)

data Direction = Down | Up | Lft | Rght deriving (Enum, Show, Eq, Bounded, Ord)

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
canTravel dir prevTile tile = case dir of
  Up -> (tile == Vert || tile == SE || tile == SW || tile == S) && (prevTile == Vert || prevTile == NE || prevTile == NW || prevTile == S)
  Down -> (tile == Vert || tile == NE || tile == NW || tile == S) && (prevTile == Vert || prevTile == SE || prevTile == SW || prevTile == S)
  Lft -> (tile == Horiz || tile == SE || tile == NE || tile == S) && (prevTile == Horiz || prevTile == SW || prevTile == NW || prevTile == S)
  Rght -> (tile == Horiz || tile == SW || tile == NW || tile == S) && (prevTile == Horiz || prevTile == SE || prevTile == NE || prevTile == S)

travel :: (Int, Int) -> Direction -> (Int, Int)
travel (x, y) dir = case dir of
  Up -> (x, y - 1)
  Down -> (x, y + 1)
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
  | null possibleTiles = Nothing
  | null newTiles = Nothing
  | otherwise = Just (head newTiles + 1)
  where
    currentTile = fromJust (getTile mp pos)
    possibleTiles = filter (\(d, newPos, tile) -> canTravel d currentTile tile && newPos /= prev_pos) (mapMaybe (mapFunc mp pos) [minBound .. maxBound])
    newTiles = mapMaybe (\(_, newPos, _) -> dfs mp False newPos pos) possibleTiles

dfsP2 :: [[Tile]] -> HashMap (Int, Int) Tile -> Bool -> (Int, Int) -> (Int, Int) -> HashMap (Int, Int) Tile
dfsP2 mp tiles firstRun pos prevPos
  | currentTile == S && not firstRun = insert pos currentTile tiles
  | null possibleTiles = tiles
  | null newTiles = tiles
  | otherwise = insert pos currentTile (head newTiles)
  where
    currentTile = fromJust (getTile mp pos)
    validLocation (dir, newPos, newTile) = canTravel dir currentTile newTile && newPos /= prevPos -- newPos /= prev_pos prevents going back
    tilesInBounds = mapMaybe (mapFunc mp pos) [minBound .. maxBound]
    possibleTiles = filter validLocation tilesInBounds
    newTiles = map (\(_, newPos, _) -> dfsP2 mp tiles False newPos pos) possibleTiles

mapFunc :: [[Tile]] -> (Int, Int) -> Direction -> Maybe (Direction, (Int, Int), Tile)
mapFunc mp pos dir = case getTile mp newPos of
  Just tile -> Just (dir, newPos, tile)
  Nothing -> Nothing
  where
    newPos = travel pos dir

determinS :: [[Tile]] -> (Int, Int) -> Tile
determinS mp pos = case sort (map snd possibleTiles) of
  [Down, Up] -> Vert
  [Down, Lft] -> SW
  [Down, Rght] -> SE
  [Up, Lft] -> NW
  [Up, Rght] -> NE
  [Lft, Rght] -> Vert
  _ -> Ground -- This should not be possible anyways
  where
    currentTile = fromJust (getTile mp pos)
    possibleTiles = filter (\(tile, d) -> isJust tile && canTravel d currentTile (fromJust tile)) (map (\d -> (getTile mp (travel pos d), d)) [minBound .. maxBound])

raycast :: HashMap (Int, Int) Tile -> Int -> Int -> Int -> Bool -> Tile -> Int
raycast tiles y width x isInside lastNonHorizTile
  | x >= width = 0
  | (x, y) `member` tiles && currentTile == Vert = flipBoundary currentTile
  | (x, y) `member` tiles && currentTile == Horiz = doNotFlipBoundary lastNonHorizTile
  -- Always east to west
  -- Still creates a line
  | (x, y) `member` tiles && lastNonHorizTile == SE && currentTile == NW = flipBoundary currentTile -- F J
  | (x, y) `member` tiles && lastNonHorizTile == NE && currentTile == SW = flipBoundary currentTile -- L 7
  -- Does not create a line
  | (x, y) `member` tiles && lastNonHorizTile == SE && currentTile == SW = doNotFlipBoundary currentTile -- F J
  | (x, y) `member` tiles && lastNonHorizTile == NE && currentTile == NW = doNotFlipBoundary currentTile -- L J
  | (x, y) `member` tiles = doNotFlipBoundary currentTile
  -- Count if is inside
  | isInside = doNotFlipBoundary lastNonHorizTile + 1
  | otherwise = doNotFlipBoundary lastNonHorizTile
  where
    doNotFlipBoundary = raycast tiles y width (x + 1) isInside
    flipBoundary = raycast tiles y width (x + 1) (not isInside)
    currentTile = tiles ! (x, y)

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let (mp, start) = parse contents
  let tiles = dfsP2 mp empty True start start
  let sChar = determinS mp start
  let updatedTiles = Data.HashMap.Strict.insert start sChar tiles
  putStrLn (unlines [[if (x, y) `member` tiles then printType (updatedTiles ! (x, y)) else '.' | x <- [0 .. length (head mp) - 1]] | y <- [0 .. length mp]])

  putStrLn ("Part 1:" ++ show (length tiles `div` 2))
  putStrLn ("Part 2: " ++ show (sum [raycast updatedTiles y (length (head mp)) 0 False Vert | y <- [0 .. (length mp - 1)]]))
