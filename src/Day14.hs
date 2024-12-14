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

-- number of connected components
regions :: S.Set Vec -> Int
regions s
  | Just c <- S.lookupMin s =
      let r = go c S.empty
       in 1 + regions (S.difference s r)
  | otherwise = 0
  where
    -- depth first search
    go :: Vec -> S.Set Vec -> S.Set Vec
    go c@(x, y) r
      | c `S.member` r = r
      | otherwise =
          let r' = S.insert c r
              nexts = [n | n <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], n `S.member` s]
           in foldr go r' nexts

part2 :: [Robot] -> Int -> IO ()
part2 robots n = do
  let moved = map (`moveN` n) robots
      s = S.fromList moved
      cc = regions s
      rows =
        [ [ if (x, y) `S.member` s then '#' else '.'
            | x <- [0 .. width - 1]
          ]
          | y <- [0 .. height - 1]
        ]

  when (cc < 250) $ do
    putStrLn $ "After " ++ show n ++ " seconds, " ++ show cc ++ " CCs:"
    mapM_ putStrLn rows
    putStr "Part 2: " >> print n

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day14.in"

    let robots = A.extract $ parse parseRobots "" raw

    putStr "Part 1: " >> print (part1 robots)
    mapM_ (part2 robots) [0..10000]
