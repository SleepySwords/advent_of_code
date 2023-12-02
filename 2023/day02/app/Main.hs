import Control.Concurrent (waitQSem)
import Data.List (isPrefixOf, maximumBy)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  part1
  part2

parse :: String -> [(Int, [(Int, Int, Int)])]
parse contents = do
  let linesOfFile = lines contents
  [let split = splitID x in (fst split, parseSets (snd split)) | x <- linesOfFile]

splitID :: String -> (Int, String)
splitID game = (read (drop 5 (head (splitOn ": " game))), last (splitOn ": " game))

parseSets :: String -> [(Int, Int, Int)]
parseSets sets = map parseSet (splitOn "; " sets)

parseSet :: String -> (Int, Int, Int)
parseSet set =
  (fst (parse "red"), fst (parse "green"), fst (parse "blue"))
  where
    parse colour = case filter (filterColour colour) (map parseColour (splitOn ", " set)) of
      [] -> (0, colour)
      x -> head x

parseColour :: String -> (Int, String)
parseColour string =
  (read (head cube), last cube)
  where
    cube = words string

filterColour :: String -> (Int, String) -> Bool
filterColour colour str = snd str == colour

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let games = parse contents
  let test = filter (all hasEnough . snd) games
  print (sum (map fst test))

hasEnough :: (Int, Int, Int) -> Bool
hasEnough (red, green, blue) = red <= 12 && green <= 13 && blue <= 14

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let games = parse contents
  let maxes = map multiplyMarbles games
  print (sum maxes)

multiplyMarbles :: (Int, [(Int, Int, Int)]) -> Int
multiplyMarbles (_, xs) = maximum (map red xs) * maximum (map green xs) * maximum (map blue xs)
  where
    red (r, g, b) = r
    green (r, g, b) = g
    blue (r, g, b) = b
