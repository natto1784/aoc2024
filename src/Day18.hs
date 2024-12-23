module Main where

import qualified AoC as A
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy1)
import Text.Parsec.String (Parser)

type Coord = (Int, Int)

type Queue = S.Set (Int, Coord)

parseBytes :: Parser [Coord]
parseBytes =
  ( (,)
      <$> (read <$> many1 digit)
      <*> (char ',' >> (read <$> many1 digit))
  )
    `sepEndBy1` newline

width :: Int
height :: Int
(width, height) = (71, 71)

oob :: Coord -> Bool
oob (x, y) = x < 0 || x >= width || y < 0 || y >= height

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

dijkstra :: [Coord] -> Maybe Int
dijkstra (S.fromList -> obs) = travel (S.singleton (0, (0, 0))) S.empty
  where
    travel :: Queue -> S.Set Coord -> Maybe Int
    travel q vis = do
      ((dis, c), q') <- S.minView q

      let ns = neighbours c
          q'' = foldr S.insert q' . map (dis + 1,) . filter (not . invalid) $ ns
          vis' = S.insert c vis

      if c == (width - 1, height - 1)
        then Just dis
        else travel q'' vis'
      where
        invalid :: Coord -> Bool
        invalid c = c `S.member` vis || c `S.member` obs || oob c

part1 :: [Coord] -> Int
part1 obs = fromJust $ dijkstra (take 1024 $ obs)

part2 :: [Coord] -> Coord
part2 obs = search 1025 (length obs - 1)
  where
    search :: Int -> Int -> Coord
    search a b
      | a >= b = obs !! b
      | otherwise =
          let m = (a + b) `div` 2
           in case dijkstra (take m obs) of
                Just _ -> search m b
                Nothing -> search a (m - 1)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day18.in"

    let bytes = A.extract $ parse parseBytes "" raw

    putStr "Part 1: " >> print (part1 bytes)
    putStr "Part 2: " >> print (part2 bytes)
