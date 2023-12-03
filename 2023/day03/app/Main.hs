module Main where

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let linesOfFile = lines contents
  print linesOfFile
