module Main where

import qualified AoC as A (extract, parseSigned)
import Control.Monad (when)
import qualified Data.Set as S
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy1, string)
import Text.Parsec.String (Parser)

type Vec = (Int, Int)

type Robot = (Vec, Vec)

width :: Int
height :: Int
(width, height) = (101, 103)

parseRobots :: Parser [Robot]
parseRobots = parseRobot `sepEndBy1` newline
  where
    parseRobot :: Parser Robot
    parseRobot = do
      x <- string "p=" *> (read <$> many1 digit)
      y <- char ',' *> (read <$> many1 digit)
      vx <- string " v=" *> A.parseSigned
      vy <- char ',' *> A.parseSigned

      return ((x, y), (vx, vy))

moveN :: Robot -> Int -> Vec
moveN ((x, y), (vx, vy)) n =
  ( (x + vx * n) `mod` width,
    (y + vy * n) `mod` height
  )

part1 :: [Robot] -> Int
part1 robots = length q1 * length q2 * length q3 * length q4
  where
    moved = map (`moveN` 100) robots
    q1 = [r | r@(x, y) <- moved, x > width `div` 2, y < height `div` 2]
    q2 = [r | r@(x, y) <- moved, x < width `div` 2, y < height `div` 2]
    q3 = [r | r@(x, y) <- moved, x < width `div` 2, y > height `div` 2]
    q4 = [r | r@(x, y) <- moved, x > width `div` 2, y > height `div` 2]

part2 :: [Robot] -> Int -> IO ()
part2 robots n = do
  let moved = map (`moveN` n) robots
      s = S.fromList moved
      d = map (\(x, y) -> sqrt (fromIntegral x ^ 2 + fromIntegral y ^ 2)) moved
      deviation = sd d $ mean d
      rows =
        [ [ if (x, y) `S.member` s then '#' else '.'
            | x <- [0 .. width - 1]
          ]
          | y <- [0 .. height - 1]
        ]

  when (deviation < 20) $ do
    putStr "Part 2: " >> print n
    putStrLn $ "After " ++ show n ++ " seconds:"
    mapM_ putStrLn rows
  where
    mean :: [Float] -> Float
    mean = (/) <$> sum <*> fromIntegral . length
    sd :: [Float] -> Float -> Float
    sd xs m = sqrt $ sum [(x - m) ^ 2 | x <- xs] / fromIntegral (length xs)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day14.in"

    let robots = A.extract $ parse parseRobots "" raw

    putStr "Part 1: " >> print (part1 robots)
    mapM_ (part2 robots) [0 .. 10000]
