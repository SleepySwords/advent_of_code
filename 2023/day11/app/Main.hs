import Data.HashSet
import Data.List
import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let mp = parse contents
  putStrLn $ printArray mp
  let width = length $ head $ lines contents
  let height = length $ lines contents
  let part1 = expand mp width height 2
  print (sum (Data.List.map (\arr -> euclidDistance (head arr) (last arr)) (subsets 2 (toList part1))))
  let part2 = expand mp width height 1000000
  print (sum (Data.List.map (\arr -> euclidDistance (head arr) (last arr)) (subsets 2 (toList part2))))

expand :: HashSet (Int, Int) -> Int -> Int -> Int -> HashSet (Int, Int)
expand mp width height expansion = loopRows (loopColumns mp (expansion - 1) 0 width) (expansion - 1) 0 height

parse :: String -> HashSet (Int, Int)
parse input = fromList (concat [Prelude.map (\(x, _) -> (x, y)) (Prelude.filter (\(_, str) -> str == '#') (zip [0 ..] line)) | (y, line) <- zip [0 ..] (lines input), '#' `elem` line])

hasGalaxyRow :: HashSet (Int, Int) -> Int -> Bool
hasGalaxyRow set row = not (Data.HashSet.null (Data.HashSet.filter (\(_, y) -> y == row) set))

hasGalaxyCol :: HashSet (Int, Int) -> Int -> Bool
hasGalaxyCol set col = not (Data.HashSet.null (Data.HashSet.filter (\(x, _) -> x == col) set))

loopColumns :: HashSet (Int, Int) -> Int -> Int -> Int -> HashSet (Int, Int)
loopColumns set expansion index len
  | index > mx = set
  | hasGalaxyCol set index = loopColumns set expansion (index + 1) len
  | otherwise = loopColumns newArr expansion (index + 1 + expansion) len
  where
    filtered = Data.HashSet.filter (\(x, _) -> x > index) set
    newArr = Data.HashSet.difference set filtered `Data.HashSet.union` Data.HashSet.map (\(x, y) -> (x + expansion, y)) filtered
    mx = maximum (Data.HashSet.map fst set)

loopRows :: HashSet (Int, Int) -> Int -> Int -> Int -> HashSet (Int, Int)
loopRows set expansion index len
  | index > mx = set
  | hasGalaxyRow set index = loopRows set expansion (index + 1) len
  | otherwise = loopRows newArr expansion (index + 1 + expansion) len
  where
    filtered = Data.HashSet.filter (\(_, y) -> y > index) set
    newArr = Data.HashSet.difference set filtered `Data.HashSet.union` Data.HashSet.map (\(x, y) -> (x, y + expansion)) filtered
    mx = maximum (Data.HashSet.map snd set)

printArray :: HashSet (Int, Int) -> String
printArray arr =
  unlines [[(if (x, y) `member` arr then '#' else '.') | x <- [0 .. maximum (Data.HashSet.map fst arr)]] | y <- [0 .. maximum (Data.HashSet.map snd arr)]]

combinations :: Int -> [(Int, Int)] -> [[(Int, Int)]]
combinations k ns = Data.List.filter ((k ==) . length) $ subsequences ns

-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
subsets :: Int -> [(Int, Int)] -> [[(Int, Int)]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = Data.List.map (x :) (subsets (n - 1) xs) ++ subsets n xs

euclidDistance :: (Int, Int) -> (Int, Int) -> Int
euclidDistance (x1, y1) (x2, y2) = (max x1 x2 - min x1 x2) + (max y1 y2 - min y1 y2)
