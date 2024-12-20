module Main where

import Data.Char (digitToInt)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Map.Lazy as ML

type Segment = (Int, Int)

-- TODO: improve this

freeSegs :: [Segment] -> [Segment]
freeSegs (_ : y : xs) = y : freeSegs xs
freeSegs _ = []

fileSegs :: [Segment] -> [Segment]
fileSegs (x : _ : xs) = x : fileSegs xs
fileSegs [x] = [x]
fileSegs _ = []

segmentMap :: [Segment] -> M.Map Int Segment
segmentMap = M.fromList . zipWith (\id (i, f) -> (i, (id, f))) [0 ..]

checksum :: M.Map Int Segment -> Int
checksum = M.foldrWithKey (\idx (id, file) -> (+) $ sum (map (id *) [idx .. idx + file - 1])) 0

part1 :: M.Map Int Segment -> [Segment] -> Int
part1 m = checksum . go m
  where
    go :: M.Map Int Segment -> [Segment] -> M.Map Int Segment
    go m [] = m
    go m ((i, free) : fs)
      | i >= j = m
      | free == 0 = go m fs
      | free > file = go (M.deleteMax $ M.insert i (id, file) m) ((i + file, free - file) : fs)
      | file > free = go (M.insert j (id, file - free) $ M.insert i (id, free) m) fs
      | free == file = go (M.deleteMax $ M.insert i (id, file) m) fs
      | otherwise = m
      where
        (j, (id, file)) = M.findMax m

part2 :: M.Map Int Segment -> M.Map Int Int -> Int
part2 files frees = checksum $ go files frees (M.size files - 1)
  where
    go :: M.Map Int Segment -> M.Map Int Int -> Int -> M.Map Int Segment
    go files _ (-1) = files
    go files frees idx = case first of
      Nothing -> go files frees (idx - 1)
      Just (i, free) ->
        go
          (M.delete j $ M.insert i (id, file) files)
          (M.delete i $ M.insert (i + file) (free - file) frees)
          idx
      where
        (j, (id, file)) = M.elemAt idx files
        first = find (\(i, free) -> free >= file && i < j) $ ML.assocs frees

main :: IO ()
main =
  do
    raw <- map digitToInt . head . lines <$> readFile "./inputs/day9.in"
    let segments = zip (0 : scanl1 (+) raw) raw
        files = fileSegs segments
        frees = freeSegs segments
        fileMap = segmentMap files
        freeMap = M.fromList frees

    putStr "Part 1: " >> print (part1 fileMap frees)
    putStr "Part 2: " >> print (part2 fileMap freeMap)
