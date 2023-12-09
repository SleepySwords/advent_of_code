import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let d = map (map read . words) (lines contents) :: [[Int]]
  let generate = map generateNext d
  putStrLn ("Part 1: " ++ show (sum (map (`findFirstByLast` 0) generate)))
  putStrLn ("Part 2: " ++ show (sum (map (`findFirstByFirst` 0) generate)))

generateNext :: [Int] -> [[Int]]
generateNext arr
  | all (== 0) arr = []
  | otherwise = arr : generateNext (map (\a -> last a - head a) (windows arr))

debug = flip trace

findFirstByLast :: [[Int]] -> Int -> Int
findFirstByLast [] ele = ele
findFirstByLast arrs ele = findFirstByLast (init arrs) (ele + lft)
  where
    lft = last (last arrs)

findFirstByFirst :: [[Int]] -> Int -> Int
findFirstByFirst [] ele = ele
findFirstByFirst arrs ele = findFirstByFirst (init arrs) (lft - ele)
  where
    lft = head (last arrs)

-- https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell
windows :: [Int] -> [[Int]]
windows arr
  | length arr < 2 = []
  | otherwise = take 2 arr : windows (tail arr)
