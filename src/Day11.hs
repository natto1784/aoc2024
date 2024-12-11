module Main where

import qualified Data.IntMap as IM

blink :: Int -> [Int]
blink 0 = [1]
blink x
  | (l, 0) <- len x `quotRem` 2,
    (a, b) <- x `quotRem` (10 ^ l) =
      [a, b]
  | otherwise = [x * 2024]
  where
    len :: Int -> Int
    len 0 = 0
    len a = 1 + len (a `div` 10)

blinkAll :: IM.IntMap Int -> IM.IntMap Int
blinkAll xs = IM.fromListWith (+) [(x', c) | (x, c) <- IM.toList xs, x' <- blink x]

blinkN :: Int -> IM.IntMap Int -> Int
blinkN n = IM.foldr (+) 0 . (!! n) . iterate blinkAll

main :: IO ()
main =
  do
    raw <- map read . words <$> readFile "./inputs/day11.in"
    let counts = IM.fromListWith (+) $ map (,1) raw

    putStr "Part 1: " >> print (blinkN 25 counts)
    putStr "Part 2: " >> print (blinkN 75 counts)
