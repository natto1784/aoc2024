module Main where

import qualified AoC as A (extract)
import Data.List (inits, tails)
import Text.Parsec (char, digit, many1, newline, parse, sepBy1, sepEndBy1)
import Text.Parsec.String (Parser)

parseReports :: Parser [[Int]]
parseReports =
  sepEndBy1
    (sepBy1 (read <$> many1 digit) (char ' '))
    newline

safe :: [Int] -> Bool
safe xs =
  let differences :: [Int]
      differences = zipWith (-) (tail xs) xs
   in all (\x -> (x >= 1) && (x <= 3)) differences || all (\x -> (x <= -1) && (x >= -3)) differences

part1 :: [[Int]] -> Int
part1 = length . filter safe

part2 :: [[Int]] -> Int
part2 = length . filter dampSafe
  where
    dampSafe :: [Int] -> Bool
    dampSafe xs = any safe (zipWith (++) (inits xs) (tail (tails xs)))

main :: IO ()
main =
  do
    raw <- readFile "./inputs/p2.in"
    -- parse the input
    let reports = A.extract $ parse parseReports "" raw
    putStr "Part 1: " >> print (part1 reports)
    putStr "Part 2: " >> print (part2 reports)
