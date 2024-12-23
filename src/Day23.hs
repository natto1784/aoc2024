module Main where

import Data.Char (chr, ord)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (intercalate, mapAccumL, maximumBy)
import Data.Ord (comparing)

type Set = S.IntSet

type Connections = M.IntMap Set

hash :: Char -> Char -> Int
hash a b = (ord a - ord 'a') * 26 + (ord b - ord 'a')

unhash :: Int -> String
unhash x = [chr $ x `div` 26 + ord 'a', chr $ x `mod` 26 + ord 'a']

parse :: [String] -> Connections
parse [] = M.empty
parse ([a, b, '-', c, d] : xs) =
  let x = hash a b
      y = hash c d
   in insert x y . insert y x $ parse xs
  where
    insert :: Int -> Int -> Connections -> Connections
    insert x y m = case x `M.lookup` m of
      Just s -> M.insert x (y `S.insert` s) m
      Nothing -> M.insert x (S.singleton y) m

bornKerbosch :: (Set -> Bool) -> Connections -> [Set]
bornKerbosch terminate connections = go S.empty (M.keysSet connections) S.empty
  where
    go :: Set -> Set -> Set -> [Set]
    go r p x
      | (S.null p && S.null x) || terminate r = [r]
      | otherwise = concat . snd . mapAccumL loop (p, x) $ S.elems p
      where
        loop :: (Set, Set) -> Int -> ((Set, Set), [Set])
        loop (p', x') v =
          let n = connections M.! v
           in ( (v `S.delete` p', v `S.insert` x'),
                go (v `S.insert` r) (p' `S.intersection` n) (x' `S.intersection` n)
              )

part1 :: Connections -> Int
part1 =
  length
    . filter (any ((== 't') . head . unhash) . S.elems)
    . filter ((== 3) . S.size)
    . bornKerbosch ((== 3) . S.size)

part2 :: Connections -> String
part2 =
  intercalate ","
    . map unhash
    . S.elems
    . maximumBy (comparing S.size)
    . bornKerbosch (const False)

main :: IO ()
main =
  do
    connections <- parse . words <$> readFile "./inputs/day23.in"

    putStr "Part 1: " >> print (part1 connections)
    putStr "Part 2: " >> print (part2 connections)
