import Data.Char (isDigit)

part1 :: IO ()
part1 = do
  contents <- readFile "inputs/main.input"
  let linesOfFile = lines contents
  let test = map convert linesOfFile
  print (sum test)

convert :: String -> Int
convert xs = read (head [[x] | x <- xs, isDigit x]) * 10 + read (last [[x] | x <- xs, isDigit x]) :: Int
