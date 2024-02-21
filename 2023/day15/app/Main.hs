import Data.Char (ord)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Sequence (Seq, index, (|>))
import qualified Data.Sequence

data Operation = Remove | Add Int deriving (Show)

parseLabel :: String -> (String, Operation)
parseLabel str =
  if "-" `isSuffixOf` str
    then (init str, Remove)
    else (init (init str), Add (read [last str]))

main :: IO ()
main = do
  contents <- readFile "inputs/main.input"
  let splits = splitOn "," (head (lines contents))
  putStrLn ("Part 1: " ++ show (sum (map (hash 0) splits)))
  let parsed = map parseLabel splits
  let boxes = Data.Sequence.fromList [Data.Sequence.empty | _ <- [0 .. 256]]
  let updated = processAll parsed boxes
  putStrLn ("Part 2: " ++ show (boxesFocalStrength updated))

hash :: Int -> String -> Int
hash = foldl (\cur x -> ((cur + ord x) * 17) `rem` 256)

processAll :: [(String, Operation)] -> Seq (Seq (String, Int)) -> Seq (Seq (String, Int))
processAll xs boxes = foldl (flip processOne) boxes xs

processOne :: (String, Operation) -> Seq (Seq (String, Int)) -> Seq (Seq (String, Int))
processOne (label, Remove) boxes = Data.Sequence.update box_index updated_box boxes
  where
    box_index = hash 0 label
    old_box = boxes `index` box_index
    updated_box = Data.Sequence.filter (\(lbl, _) -> lbl /= label) old_box
processOne (label, Add focus) boxes = Data.Sequence.update box_index updated_box boxes
  where
    box_index = hash 0 label
    old_box = boxes `index` box_index
    labelPos = Data.Sequence.findIndexL (\(lbl, _) -> lbl == label) old_box
    updated_box = case labelPos of
      Just ind -> Data.Sequence.update ind (label, focus) old_box
      Nothing -> old_box |> (label, focus)

boxFocalStrength :: Seq (String, Int) -> Int
boxFocalStrength = Data.Sequence.foldlWithIndex (\x i (_, focus) -> x + focus * (i + 1)) 0

boxesFocalStrength :: Seq (Seq (String, Int)) -> Int
boxesFocalStrength = Data.Sequence.foldlWithIndex (\x i el -> x + (i + 1) * boxFocalStrength el) 0
