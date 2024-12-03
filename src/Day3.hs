module Main where

import qualified AoC as A (extract)
import Data.Maybe (catMaybes)
import Text.Parsec (anyChar, char, digit, many1, parse, string, try, (<|>))
import Text.Parsec.String (Parser)

type Pair = (Int, Int)

data Instruction = Multiply Pair | Do | Dont deriving (Show, Eq)

parseMul :: Parser Pair
parseMul =
  string "mul("
    *> ( (,)
           <$> (read <$> many1 digit)
           <*> (char ',' *> (read <$> many1 digit) <* char ')')
       )

parseIns :: Parser [Maybe Instruction]
parseIns =
  many1
    ( try (Just . Multiply <$> parseMul)
        <|> (try (string "do()") >> return (Just Do))
        <|> (try (string "don't()") >> return (Just Dont))
        <|> (anyChar >> return Nothing)
    )

part1 :: [Instruction] -> Int
part1 = sum . map (\case Multiply (a, b) -> a * b; _ -> 0)

part2 :: [Instruction] -> Int
part2 =
  fst
    . foldl
      ( \(s, e) ins ->
          case ins of
            Multiply (a, b) | e -> (s + a * b, e)
            Do -> (s, True)
            Dont -> (s, False)
            _ -> (s, e)
      )
      (0, True) -- (sum, enabled)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day3.in"
    -- parse the input
    let instructions = catMaybes . A.extract $ parse parseIns "" raw
    putStr "Part 1: " >> print (part1 instructions)
    putStr "Part 2: " >> print (part2 instructions)
