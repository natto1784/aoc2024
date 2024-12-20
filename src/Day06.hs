module Main where

import qualified Data.Set as S

type Grid = [[Char]]

type Dimensions = (Int, Int)

type Coord = (Int, Int)

data Dir = North | East | South | West deriving (Ord, Eq, Show)

type State = (Coord, Dir)

next :: Dir -> Coord -> Coord
next North (x, y) = (x, y - 1)
next East (x, y) = (x + 1, y)
next South (x, y) = (x, y + 1)
next West (x, y) = (x - 1, y)

rotate :: Dir -> Dir
rotate North = East
rotate East = South
rotate South = West
rotate West = North

getPath :: S.Set Coord -> Dimensions -> Coord -> S.Set Coord
getPath obs (m, n) start = go S.empty start North
  where
    oob :: Coord -> Bool
    oob (x, y) = x < 0 || y < 0 || x >= m || y >= n

    go :: S.Set Coord -> Coord -> Dir -> S.Set Coord
    go path c d
      | oob n = S.insert c path
      | n `S.member` obs = go path c (rotate d)
      | otherwise = go (S.insert c path) n d
      where
        n = next d c

isLoop :: S.Set Coord -> Dimensions -> Coord -> Bool
isLoop obs (m, n) start = go S.empty start North
  where
    oob :: Coord -> Bool
    oob (x, y) = x < 0 || y < 0 || x >= m || y >= n

    go :: S.Set State -> Coord -> Dir -> Bool
    go states c d
      | oob n = False
      | (c, d) `S.member` states = True
      | n `S.member` obs = go states c (rotate d)
      | otherwise = go (S.insert (c, d) states) n d
      where
        n = next d c

main :: IO ()
main =
  do
    grid <- lines <$> readFile "./inputs/day6.in"

    let m = length grid
        n = length $ head grid

    let grid' = [((x, y), ch) | (y, row) <- zip [0 ..] grid, (x, ch) <- zip [0 ..] row]
        obstacles = S.fromList [coord | (coord, ch) <- grid', ch == '#']
        start = head [coord | (coord, ch) <- grid', ch == '^']
        path = getPath obstacles (m, n) start

    let part1 = S.size path
        part2 =
          S.size $
            S.filter
              (\p -> isLoop (S.insert p obstacles) (m, n) start)
              (S.delete start path)

    putStr "Part 1: " >> print part1
    putStr "Part 2: " >> print part2
