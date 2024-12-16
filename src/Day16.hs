module Main where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

type Grid = M.Map Coord Char

data Dir = East | West | North | South deriving (Eq, Ord, Show)

type State = (Coord, Dir)

type Queue = S.Set (Int, Coord, Dir)

dirs :: [Dir]
dirs = [East, West, North, South]

next :: Coord -> Dir -> Coord
next (x, y) East = (x + 1, y)
next (x, y) West = (x - 1, y)
next (x, y) North = (x, y - 1)
next (x, y) South = (x, y + 1)

rotate :: Dir -> Dir
rotate East = South
rotate South = West
rotate West = North
rotate North = East

rotate' :: Dir -> Dir
rotate' East = North
rotate' North = West
rotate' West = South
rotate' South = East

dijkstra :: Queue -> Char -> Grid -> M.Map State Int
dijkstra queue end grid = case travel queue M.empty of
  Just distances -> distances
  Nothing -> error "what?"
  where
    travel :: Queue -> M.Map State Int -> Maybe (M.Map State Int)
    travel q dist = do
      ((dis, c, dir), q') <- S.minView q

      let q'' =
            S.insert (dis + 1000, c, rotate dir) $
              S.insert (dis + 1000, c, rotate' dir) q'
          n = next c dir
          dist' = M.insert (c, dir) dis dist

      ch <- M.lookup n grid

      if
        | (c, dir) `M.member` dist -> if S.null q' then Just dist else travel q' dist
        | ch `elem` [end, '.'] -> travel (S.insert (dis + 1, n, dir) q'') dist'
        | otherwise -> travel q'' dist'

part1 :: Grid -> Coord -> Coord -> Int
part1 grid start end = minimum [dis | ((c, _), dis) <- M.assocs dist, c == end]
  where
    dist :: M.Map State Int
    dist = dijkstra (S.fromList [(0, start, East)]) 'E' grid

part2 :: Grid -> Coord -> Coord -> Int -> Int
part2 grid start end cost =
  (+ 2) . S.size . S.fromList $
    [ c
      | (c, ch) <- M.assocs grid,
        ch == '.',
        dir <- dirs,
        let Just d1 = M.lookup (c, dir) dist,
        let Just d2 = M.lookup (c, (rotate . rotate) dir) dist',
        d1 + d2 == cost
    ]
  where
    dist :: M.Map State Int
    dist = dijkstra (S.fromList [(0, start, East)]) 'E' grid

    dist' :: M.Map State Int
    dist' = dijkstra (S.fromList [(0, end, dir) | dir <- dirs]) 'S' grid

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day16.in"

    let grid = M.fromList [((x, y), ch) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row]
        [start] = [(x, y) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row, ch == 'S']
        [end] = [(x, y) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row, ch == 'E']
        cost = part1 grid start end

    putStr "Part 1: " >> print cost
    putStr "Part 2: " >> print (part2 grid start end cost)
