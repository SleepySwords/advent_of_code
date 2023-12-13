import Data.List.Split (splitOn)
import Debug.Trace (trace)

data State = Operating | Damaged | Unkown deriving (Enum, Eq)

parse :: String -> ([State], [Int])
parse str = (map getType $ head split, map read nums)
  where
    split = splitOn " " str
    nums = splitOn "," (last split)

getType :: Char -> State
getType ch
  | ch == '.' = Operating
  | ch == '#' = Damaged
  | ch == '?' = Unkown
  | otherwise = Unkown

instance Show State where
  show Operating = "."
  show Damaged = "#"
  show Unkown = "?"

isValid :: [State] -> [Int] -> Bool
isValid states values = frequency states == values

perms :: Int -> Int -> [[State]]
perms total damaged
  | total < 0 || damaged < 0 = []
  | total == damaged = [[Damaged | _ <- [0 .. total - 1]]]
  | damaged == 0 = [[Operating | _ <- [0 .. total - 1]]]
  | otherwise = map (Operating :) (perms (total - 1) damaged) ++ map (Damaged :) (perms (total - 1) (damaged - 1))

updateState :: [State] -> [State] -> [State]
updateState [] _ = []
updateState states perm
  | head states == Unkown = head perm : updateState (tail states) (tail perm)
  | otherwise = head states : updateState (tail states) perm

frequency :: [State] -> [Int]
frequency [] = []
frequency states =
  if head states == Operating
    then frequency (tail states)
    else length (takeWhile (== Damaged) states) : frequency (dropWhile (== Damaged) states)

validCombinations :: [State] -> [Int] -> [[State]]
validCombinations states values = filter (`isValid` values) $ map (updateState states) (perms total damaged)
  where
    damaged = sum values - length (filter (== Damaged) states)
    total = length (filter (== Unkown) states)

-- unfoldInput :: ([State], [Int]) -> ([State], [Int])
-- unfoldInput (states, values) = (joinState [states | _ <- [0 .. 4]], concat [values | _ <- [0 .. 4]])

unfoldLastInput :: ([State], [Int]) -> ([State], [Int])
unfoldLastInput (states, values) = (takeWhile (== Damaged) (reverse states) ++ Unkown : states, if len == 0 then values else len : values)
  where
    len = length (takeWhile (== Damaged) (reverse states))

unfoldLastInput2 :: ([State], [Int]) -> [([State], [Int])]
unfoldLastInput2 (states, values) = map (\a -> (a ++ Unkown : states, values ++ values)) test
  where
    valuesRange = [1 .. length values]
    -- DP this portion and make it so that it goes through each case?
    test = concatMap (\a -> validCombinations states (take a values)) valuesRange

debug = flip trace

unfoldLastInput3 :: ([State], [Int]) -> ([State], [Int])
unfoldLastInput3 (states, values) = (states ++ Unkown : states, values ++ values)

-- join states =

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let input = map parse (lines contents)
  let part1 = map (length . uncurry validCombinations) input
  print part1

  let input = map (unfoldLastInput2 . parse) (lines contents)
  let part2Partial = map (sum . map (length . uncurry validCombinations)) input
  -- let part2Partial = map (map (length . uncurry validCombinations)) input
  print part2Partial
  print "completed"
  print $ sum (map (\(p1, p2) -> (p2)) (zip part1 part2Partial))
