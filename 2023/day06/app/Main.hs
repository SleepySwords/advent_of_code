module Main where

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let (time, dist) = let splits = map words $ lines contents in (map read $ tail $ head splits, map read $ tail $ last splits) :: ([Int], [Int])

  let records = [(time !! x, dist !! x) | x <- [0 .. length time - 1]]

  putStrLn $ "Part 1: " ++ show (product (map findTries records))

part2 :: IO ()
part2 = do
  contents <- readFile "inputs/main.input"
  let (time, dist) = let splits = map words $ lines contents in (read $ concat $ tail $ head splits, read $ concat $ tail $ last splits) :: (Int, Int)

  putStrLn $ "Part 2: " ++ show (findTries (time, dist))

numberTries :: (Int, Int) -> Int -> Int
numberTries (maxTime, dist) holdTime
  | holdTime > maxTime = 0
  | distance maxTime holdTime > dist = 1 + numberTries (maxTime, dist) (holdTime + 1)
  | otherwise = numberTries (maxTime, dist) (holdTime + 1)

distance :: Int -> Int -> Int
distance maxTime holdTime = (maxTime - holdTime) * holdTime

findTries :: (Int, Int) -> Int
findTries (maxTime, dist) = floor (uncurry max quadrat) - ceiling (uncurry min quadrat) + 1
  where
    quadrat = quadratic (-(1.0 :: Double)) (fromIntegral maxTime) (fromIntegral (-dist))

quadratic :: (Floating a) => a -> a -> a -> (a, a)
quadratic a b c = ((-b - discrim) / 2 * a, (-b + discrim) / 2 * a)
  where discrim = sqrt (b * b - 4 * a * c)
