module AoC where

import Data.List (transpose)
import Text.Parsec (ParseError)

-- extract Right value after parsing
extract :: Either ParseError a -> a
extract (Left err) = error ("Parsing failed: " ++ show err)
extract (Right val) = val

-- count elements in a list
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

-- extract diagonals from a matrix
diagonals :: [[a]] -> [[a]]
diagonals =
  (++)
    <$> reverse . transpose . zipWith drop [0 ..]
    <*> transpose . zipWith drop [1 ..] . transpose

-- get indices of substring sub in str
findSubstrings :: [Char] -> [Char] -> [Int]
findSubstrings sub str = findSubstrings' sub str 0
  where
    findSubstrings' _ [] _ = []
    findSubstrings' sub str@(x : xs) idx
      | take (length sub) str == sub = idx : findSubstrings' sub xs (idx + 1)
      | otherwise = findSubstrings' sub xs (idx + 1)
