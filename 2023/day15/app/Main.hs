import Data.Char (ord)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Sequence (Seq, index, (|>))
import qualified Data.Sequence as Sequence

data Operation = Remove | Add Int deriving (Show)

parseLabel :: String -> (String, Operation)
parseLabel str =
  if "-" `isSuffixOf` str
    then (lbl, Remove)
    else (init lbl, Add $ read [last str])
  where
    lbl = init str

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let splits = splitOn "," (head (lines contents))
  putStrLn ("Part 1: " ++ show (sum . map hash $ splits))
  let parsed = map parseLabel splits
  let boxes = Sequence.fromList [Sequence.empty | _ <- [0 .. 256]]
  let updated = processAll parsed boxes
  putStrLn ("Part 2: " ++ show (boxesFocalStrength updated))

hash :: String -> Int
hash = foldl (\cur x -> ((cur + ord x) * 17) `rem` 256) 0

processAll :: [(String, Operation)] -> Seq (Seq (String, Int)) -> Seq (Seq (String, Int))
processAll xs boxes = foldl (flip processOne) boxes xs

processOne :: (String, Operation) -> Seq (Seq (String, Int)) -> Seq (Seq (String, Int))
processOne (label, Remove) boxes = Sequence.update box_index updated_box boxes
  where
    box_index = hash label
    old_box = boxes `index` box_index
    updated_box = Sequence.filter ((/= label) . fst) old_box
processOne (label, Add focus) boxes = Sequence.update box_index updated_box boxes
  where
    box_index = hash label
    old_box = boxes `index` box_index
    labelPos = Sequence.findIndexL ((== label) . fst) old_box
    updated_box = case labelPos of
      Just ind -> Sequence.update ind (label, focus) old_box
      Nothing -> old_box |> (label, focus)

boxFocalStrength :: Seq (String, Int) -> Int
boxFocalStrength = Sequence.foldlWithIndex (\x i (_, focus) -> x + focus * (i + 1)) 0

boxesFocalStrength :: Seq (Seq (String, Int)) -> Int
boxesFocalStrength = Sequence.foldlWithIndex (\x i el -> x + (i + 1) * boxFocalStrength el) 0
