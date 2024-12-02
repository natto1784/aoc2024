module Main where

import qualified AoC as A (count, extract)
import Data.List (sort)
import Text.Parsec (digit, many1, newline, parse, sepEndBy1, space)
import Text.Parsec.String (Parser)

parseLists :: Parser ([Int], [Int])
parseLists =
  unzip
    -- multiple lines
    <$> sepEndBy1
      ( (,)
          -- first number
          <$> (read <$> many1 digit)
          -- second number
          <*> (many1 space *> (read <$> many1 digit))
      )
      newline

part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ zipWith ((abs .) . (-)) xs ys

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum $ map (\x -> x * A.count x ys) xs

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day1.in"
    -- parse the input
    let (xs', ys') = A.extract $ parse parseLists "" raw
    -- sort the lists
    let (xs, ys) = (sort xs', sort ys')
    putStr "Part 1: " >> print (part1 xs ys)
    putStr "Part 2: " >> print (part2 xs ys)
