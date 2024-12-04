module Main where

import qualified AoC as A (diagonals, subArrays)
import Data.List (isPrefixOf, tails, transpose)

part1 :: [[Char]] -> Int
part1 grid =
  (sum . concatMap countXmas)
    [ grid,
      transpose grid,
      A.diagonals grid,
      A.diagonals $ map reverse grid
    ]
  where
    countXmas :: [[Char]] -> [Int]
    countXmas = map (length . filter ((||) <$> isPrefixOf "XMAS" <*> isPrefixOf "SAMX") . tails)

part2 :: [[Char]] -> Int
part2 grid =
  let groups = concat $ A.subArrays 3 $ transpose $ A.subArrays 3 grid
   in length $ filter check groups
  where
    check :: [[Char]] -> Bool
    check
      [ [a, _, b],
        [_, 'A', _],
        [c, _, d]
        ] = elem [a, d] ["MS", "SM"] && elem [b, c] ["MS", "SM"]
    check _ = False

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day4.in"
    putStr "Part 1: " >> print (part1 raw)
    putStr "Part 2: " >> print (part2 raw)
