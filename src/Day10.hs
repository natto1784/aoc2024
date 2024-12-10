module Main where

import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

type Grid = M.Map (Int, Int) Int

data Dir = North | South | East | West deriving (Show)

next :: Coord -> Dir -> Coord
next (x, y) North = (x, y - 1)
next (x, y) South = (x, y + 1)
next (x, y) East = (x + 1, y)
next (x, y) West = (x - 1, y)

part1 :: Grid -> Int
part1 grid = sum . map (S.size . go 0 S.empty) . M.keys . M.filter (== 0) $ grid
  where
    go :: Int -> S.Set Coord -> Coord -> S.Set Coord
    go h s c
      | h == 9 = S.insert c s
      | otherwise =
          S.unions
            [ go h' s n
              | d <- [North, South, East, West],
                let n = next c d,
                Just h' <- [M.lookup n grid],
                h' == h + 1
            ]

part2 :: Grid -> Int
part2 grid = sum . map (go 0) . M.keys . M.filter (== 0) $ grid
  where
    go :: Int -> Coord -> Int
    go h c
      | h == 9 = 1
      | otherwise =
          sum
            [ go h' n
              | d <- [North, South, East, West],
                let n = next c d,
                Just h' <- [M.lookup n grid],
                h' == h + 1
            ]

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day10.in"

    let grid =
          M.fromList
            [ ((x, y), digitToInt ch)
              | (y, row) <- zip [0 ..] raw,
                (x, ch) <- zip [0 ..] row
            ]

    putStr "Part 1: " >> print (part1 grid)
    putStr "Part 2: " >> print (part2 grid)