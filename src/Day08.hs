module Main where

import qualified Data.Set as S

type Grid = [((Int, Int), Char)]

part1 :: Int -> Int -> Grid -> Int
part1 m n grid =
  S.size . S.fromList $
    [ (x, y)
      | (c1@(x1, y1), ch1) <- grid,
        (c2@(x2, y2), ch2) <- grid,
        ch1 == ch2,
        c1 /= c2,
        let x = 2 * x1 - x2,
        let y = 2 * y1 - y2,
        x >= 0 && y >= 0 && x < n && y < m
    ]

part2 :: Int -> Int -> Grid -> Int
part2 m n grid =
  S.size . S.fromList $
    [ (x, y)
      | (c1@(x1, y1), ch1) <- grid,
        (c2@(x2, y2), ch2) <- grid,
        ch1 == ch2,
        c1 /= c2,
        let xs = takeWhile (\x -> x >= 0 && x < n) [x1, 2 * x1 - x2 ..],
        let ys = takeWhile (\y -> y >= 0 && y < m) [y1, 2 * y1 - y2 ..],
        (x, y) <- zip xs ys
    ]

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day8.in"

    let m = length raw
        n = length $ head raw

    let grid = [((x, y), ch) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row, ch /= '.']

    putStr "Part 1: " >> print (part1 m n grid)
    putStr "Part 2: " >> print (part2 m n grid)
