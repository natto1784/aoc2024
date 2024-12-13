module Main where

import qualified AoC as A (extract)
import Control.Monad (void)
import Text.Parsec (digit, eof, many1, newline, parse, sepBy1, string, try, (<|>))
import Text.Parsec.String (Parser)

type Pair = (Int, Int)

type Conf = (Pair, Pair, Pair)

parseConfs :: Parser [Conf]
parseConfs = parseConf `sepBy1` newline
  where
    parseConf :: Parser Conf
    parseConf = do
      ax <- string "Button A: X+" *> (read <$> many1 digit)
      ay <- string ", Y+" *> (read <$> many1 digit)
      _ <- newline

      bx <- string "Button B: X+" *> (read <$> many1 digit)
      by <- string ", Y+" *> (read <$> many1 digit)
      _ <- newline

      px <- string "Prize: X=" *> (read <$> many1 digit)
      py <- string ", Y=" *> (read <$> many1 digit)
      try (void newline <|> eof)

      return ((ax, ay), (bx, by), (px, py))

-- the matrix is never singular somehow, wtf aoc!
soln :: Conf -> Maybe (Int, Int)
soln ((ax, ay), (bx, by), (cx, cy))
  | det == 0 = error "Singular Matrix Encountered, |A| = 0"
  | (b, 0) <- (cy * ax - cx * ay) `quotRem` det, -- remainder 0 for integer solns
    (a, 0) <- (cy - by * b) `quotRem` ay =
      Just (a, b)
  | otherwise = Nothing
  where
    det = ax * by - bx * ay

part1 :: [Conf] -> Int
part1 confs =
  sum
    [ 3 * a + b
      | conf <- confs,
        Just (a, b) <- [soln conf],
        a <= 100 && b <= 100
    ]

transform :: Conf -> Conf
transform (a, b, (x, y)) = (a, b, (x + 10000000000000, y + 10000000000000))

part2 :: [Conf] -> Int
part2 confs =
  sum
    [ 3 * a + b
      | conf <- map transform confs,
        Just (a, b) <- [soln conf]
    ]

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day13.in"
    let confs = A.extract $ parse parseConfs "" raw

    putStr "Part 1: " >> print (part1 confs)
    putStr "Part 2: " >> print (part2 confs)
