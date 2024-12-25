module Main where

import Data.List (partition)

group :: [[a]] -> [[[a]]]
group [] = []
group ([] : xs) = group xs
group xs = take 7 xs : group (drop 7 xs)

part1 :: [[String]] -> [[String]] -> Int
part1 locks keys =
  length
    [ ()
      | lock <- locks,
        key <- keys,
        and
          [ l == '.' || k == '.'
            | (lrow, krow) <- zip lock key,
              (l, k) <- zip lrow krow
          ]
    ]

main :: IO ()
main =
  do
    (locks, keys) <-
      partition ((== '#') . head . head)
        . group
        . lines
        <$> readFile "./inputs/day25.in"

    putStr "Part 1: " >> print (part1 locks keys)
    putStrLn "Part 2: See you next year :D"
