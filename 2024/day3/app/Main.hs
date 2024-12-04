{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, many, parse, single, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

data Command = Multiply (Int, Int) | Do | Dont deriving (Show)

type Parser a = Parsec Void Text a

parseMul :: Parser Command
parseMul = Multiply <$> ((,) <$ string "mul(" <*> decimal <* single ',' <*> decimal <* single ')')

parseDo :: Parser Command
parseDo = Do <$ string "do()"

parseDont :: Parser Command
parseDont = Dont <$ string "don't()"

parseMultiplMul :: Parser [Maybe Command]
parseMultiplMul = many ((Just <$> try (parseMul <|> try parseDont <|> parseDo)) <|> (Nothing <$ anySingle))

main :: IO ()
main = do
  contents <- getContents
  let commands = concatMap parseCommands (lines contents)
  putStrLn $ "Part 1: " ++ show (evaluateCommandsPt1 commands)
  putStrLn $ "Part 2: " ++ show (evaluateCommandsPt2 commands)

parseCommands :: String -> [Command]
parseCommands st = case parse parseMultiplMul "" (pack st) of
  Left ok -> trace (show ok) []
  Right ok -> catMaybes ok

evaluateCommandsPt1 :: [Command] -> Int
evaluateCommandsPt1 st = sum $ map evaluateCommand st
  where
    evaluateCommand (Multiply (a, b)) = a * b
    evaluateCommand _ = 0

evaluateCommandsPt2 :: [Command] -> Int
evaluateCommandsPt2 st = sum $ foldl evaluateCommand (True, 0) st
  where
    evaluateCommand (toadd, sum) (Multiply (a, b)) = (toadd, sum + if toadd then a * b else 0)
    evaluateCommand (_, sum) Do = (True, sum)
    evaluateCommand (_, sum) Dont = (False, sum)
