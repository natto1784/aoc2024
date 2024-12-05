module Main where

import qualified AoC as A (extract)
import Data.Bool (bool)
import Data.List (sortBy, tails)
import qualified Data.Set as S
import Text.Parsec (char, digit, many1, newline, parse, sepBy1, sepEndBy1)
import Text.Parsec.String (Parser)

type Rule = (Int, Int)

type Rules = S.Set Rule

type Update = [Int]

parseRules :: Parser (Rules, [Update])
parseRules = do
  rules <- S.fromList <$> parseRule `sepEndBy1` newline
  newline
  updates <- parseUpdate `sepEndBy1` newline
  return (rules, updates)
  where
    parseRule :: Parser Rule
    parseRule = (,) <$> (read <$> many1 digit <* char '|') <*> (read <$> many1 digit)
    parseUpdate :: Parser Update
    parseUpdate = (read <$> many1 digit) `sepBy1` char ','

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

isOrdered :: Rules -> Update -> Bool
isOrdered rules (first : rest) = all (flip S.member rules . (,) first) rest
isOrdered _ _ = False

part1 :: Rules -> [Update] -> Int
part1 rules = sum . map middle . filter (all (isOrdered rules) . init . tails)

part2 :: Rules -> [Update] -> Int
part2 rules =
  sum
    . map (middle . sortBy (\x -> bool GT LT . flip S.member rules . (,) x))
    . filter (not . all (isOrdered rules) . init . tails)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day5.in"
    let (rules, updates) = A.extract $ parse parseRules "" raw
    putStr "Part 1: " >> print (part1 rules updates)
    putStr "Part 2: " >> print (part2 rules updates)
