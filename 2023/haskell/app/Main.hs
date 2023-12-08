main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  print contents
