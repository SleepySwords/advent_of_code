module Main where

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let (time, dist) = let splits = map words $ lines contents in (map read $ tail $ head splits, map read $ tail $ last splits) :: ([Int], [Int])

  let records = [(time!!x, dist!!x) | x <- [0..length time - 1]]

  putStrLn $ "Part 1: " ++ show (product (map (`numberTries` 0) records))

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let (time, dist) = let splits = map words $ lines contents in (read $ concat $ tail $ head splits, read $ concat $ tail $ last splits) :: (Int, Int)

  putStrLn $ "Part 2: " ++ show (numberTries (time, dist) 0)


numberTries :: (Int, Int) -> Int -> Int
numberTries (maxTime, dist) holdTime
  | holdTime > maxTime = 0
  | distance maxTime holdTime > dist = 1 + numberTries (maxTime, dist) (holdTime + 1)
  | otherwise = numberTries (maxTime, dist) (holdTime + 1)

distance :: Int -> Int -> Int
distance maxTime holdTime = (maxTime - holdTime) * holdTime
