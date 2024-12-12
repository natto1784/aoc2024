module Main where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

type Grid = M.Map Coord Char

type Region = S.Set Coord

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

regions :: Grid -> [Region]
regions m
  | Just (c, ch) <- M.lookupMin m =
      let r = region m c ch
       in r : regions (M.withoutKeys m r)
  | otherwise = []
  where
    region :: Grid -> (Int, Int) -> Char -> Region
    region grid c ch = go c S.empty
      where
        -- depth first search
        go :: Coord -> Region -> Region
        go c s
          | c `S.member` s = s
          | otherwise =
              let s' = S.insert c s
                  nexts = [n | n <- neighbours c, M.lookup n grid == Just ch]
               in foldr go s' nexts

perimeter :: Region -> Int
perimeter r = S.foldl (\a -> (+ a) <$> length . filter (not . (`S.member` r)) . neighbours) 0 r

corners :: Region -> Int
corners r = sum . map corners' $ S.toList r
  where
    corners' :: Coord -> Int
    corners' c =
      let d@[_, _, n, s] = neighbours c
          [nw, ne, _, _] = neighbours n
          [sw, se, _, _] = neighbours s
          [w', e', n', s'] = map (not . (`S.member` r)) d
          [nw', ne', sw', se'] = map (`S.member` r) [nw, ne, sw, se]
       in length $
            filter
              id
              [ n' && (e' || ne'),
                e' && (s' || se'),
                s' && (w' || sw'),
                w' && (n' || nw')
              ]

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day12.in"

    let grid =
          M.fromList
            [ ((x, y), ch)
              | (y, row) <- zip [0 ..] raw,
                (x, ch) <- zip [0 ..] row
            ]
        rs = regions grid

    putStr "Part 1: " >> print (sum . map ((*) <$> perimeter <*> S.size) $ rs)
    putStr "Part 2: " >> print (sum . map ((*) <$> corners <*> S.size) $ rs)
