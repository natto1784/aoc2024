module Main where

import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

type Grid = M.Map (Int, Int) Int

part1 :: Grid -> Int
part1 grid = sum . map (S.size . go 0 S.empty) . M.keys . M.filter (== 0) $ grid
  where
    go :: Int -> S.Set Coord -> Coord -> S.Set Coord
    go h s c@(x, y)
      | h == 9 = S.insert c s
      | otherwise =
          S.unions
            [ go h' s n
              | n <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)],
                Just h' <- [M.lookup n grid],
                h' == h + 1
            ]

part2 :: Grid -> Int
part2 grid = sum . map (go 0) . M.keys . M.filter (== 0) $ grid
  where
    go :: Int -> Coord -> Int
    go h (x, y)
      | h == 9 = 1
      | otherwise =
          sum
            [ go h' n
              | n <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)],
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
