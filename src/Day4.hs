module Main where

import qualified AoC as A (diagonals, findSubstrings)
import Data.List (intersect, transpose)

-- |
--   A.diagonals gives the diagonals of a matrix
--   A.findSubstrings provides the indices of a substring in a string
--   We get the number of substrings XMAS and SAMX in all possible ways and add
part1 :: [[Char]] -> Int
part1 grid =
  (sum . concat)
    [ map countXmas grid,
      map countXmas $ transpose grid,
      map countXmas $ A.diagonals grid,
      map countXmas $ A.diagonals $ map reverse grid
    ]
  where
    countXmas :: [Char] -> Int
    countXmas =
      (+)
        <$> length . A.findSubstrings "XMAS"
        <*> length . A.findSubstrings "SAMX"

-- |
--   We get indices of substrings MAS and SAM across only the diagonals
--   Then we calculate the coordinates of the letter 'A' in those
--   Then we intersect the lists to find common As
part2 :: [[Char]] -> Int
part2 grid =
  let m = length grid
      n = length $ head grid
      -- diagonals = left -> right, top -> bottom
      diags1 = findMas $ A.diagonals grid
      -- diagonals = right -> left, top -> bottom
      diags2 = findMas $ A.diagonals $ map reverse grid
   in length $
        intersect
          -- coordinate calculation for A across diag1 and diag2
          [ if i < n
              then (n - i + diag, diag + 1)
              else (diag + 1, i - n + diag + 2)
            | (i, diags) <- zip [0 ..] diags1,
              diag <- diags
          ]
          [ if i < m
              then (i - diag - 1, diag + 1)
              else (m - diag - 2, i - m + diag + 2)
            | (i, diags) <- zip [0 ..] diags2,
              diag <- diags
          ]
  where
    -- find indices of MAS and SAM across diagonals
    findMas :: [[Char]] -> [[Int]]
    findMas = map ((++) <$> A.findSubstrings "MAS" <*> A.findSubstrings "SAM")

main :: IO ()
main =
  do
    raw <- lines <$> readFile "./inputs/day4.in"

    putStr "Part 1: " >> print (part1 raw)
    putStr "Part 2: " >> print (part2 raw)
