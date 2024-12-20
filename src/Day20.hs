module Main where

import qualified Data.IntMap as IM
import qualified Data.Map as M

type Coord = (Int, Int)

type Grid = M.Map Coord Char

type Path = IM.IntMap Coord

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

race :: Grid -> Coord -> Path
race grid = IM.fromList . zip [0 ..] . path (-1, -1)
  where
    path :: Coord -> Coord -> [Coord]
    path prev c = case ch of
      'E' -> [c, next]
      '.' -> c : path c next
      where
        [(next, ch)] =
          [ (n, ch)
            | n <- neighbours c,
              n /= prev,
              let Just ch = n `M.lookup` grid,
              ch /= '#'
          ]

nCheats :: Path -> Int -> Int
nCheats path n =
  let l = IM.size path
   in length
        [ ()
          | i <- [0 .. l - 1],
            j <- [i + 100 .. l - 1],
            let d = manhattan (path IM.! i) (path IM.! j),
            d <= n,
            100 <= j - i - d + 1
        ]

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day20.in"

    let [start] = [(x, y) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row, ch == 'S']
        grid = M.fromList [((x, y), ch) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row]
        path = race grid start

    putStr "Part 1: " >> print (nCheats path 2)
    putStr "Part 2: " >> print (nCheats path 20)
